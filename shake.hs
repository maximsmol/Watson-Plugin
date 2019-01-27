{-# LANGUAGE TypeApplications #-} -- generic-lens
{-# LANGUAGE DataKinds #-} -- type-level strings
{-# LANGUAGE DeriveGeneric #-} -- generics
{-# LANGUAGE DeriveAnyClass #-} -- data-default
{-# LANGUAGE DuplicateRecordFields #-} -- generic-lens only handles the use site

import Control.Monad(forM_, join)
import Data.List(group, sort, (\\), stripPrefix)
import Data.Maybe(fromMaybe)

import System.FilePath.Glob
import GHC.Generics
import Data.Default
import Control.Lens hiding ((<.>))
import Data.Generics.Product
import System.Directory(createDirectoryIfMissing, getCurrentDirectory)
import Data.List.Extra(stripSuffix)

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util


projectCfg :: ProjectCfg
projectCfg = def

main :: IO ()
main = shakeArgs (shakeOptions' (def :: DirCfg)) $ do
  cwd <- liftIO $ getCurrentDirectory
  let dircfg = projectCfg^.field @"dircfg"

  action $ do
    liftIO $ createDirectoryIfMissing True (dircfg^.field @"bld"</>"report")

  let watsonPluginCPPTargetCfg =
        (def :: CPPTargetCfg)
          & field @"name" .~ "base"
  let watsonPluginCPPSourceCfg =
        (def :: CPPSourceCfg)
          & field @"fromInTarget" .~ ((<.> "cpp") <$> ["MMPlugin", "base", "log"])
  let watsonPluginCPPIncludeListing =
        (def :: CPPIncludeListing)
          & field @"fromInTarget" .~ ["."]
  let sdkIncludes = (("sdk"</>"hl2sdk-tf2"</>"public") </>) <$> [".", "engine", "mathlib", "vstdlib", "tier0", "tier1", "game"</>"server"]
  let watsonPluginCPPSysIncludeListing =
        (def :: CPPIncludeListing)
          & field @"fromWorld" .~ ["mmsource"</>"core", "mmsource"</>"core"</>"sourcehook"] ++ sdkIncludes
  let watsonPluginCPPIncludeCfg =
        (def :: CPPIncludeCfg)
          & field @"warn" .~ watsonPluginCPPIncludeListing
          & field @"noWarn" .~ watsonPluginCPPSysIncludeListing
  let watsonPluginCPPObjBuildFlagCfg =
        defaultCPPObjectBuildFlagCfg
          & field @"common" <>~ (("-D"++) <$> ["POSIX", "OSX"])
          & field @"common" <>~ ((\x -> "-D" ++ x^.field @"define" ++ "=" ++ (show $ x^.field @"code")) <$> sdks)
          & field @"common" <>~ ["-DSOURCE_ENGINE=" ++ show 11]
          & field @"common" <>~ (("-D"++) <$> ["_vsnprintf=vsnprintf", "stricmp=strcasecmp"])

          & field @"common" <>~ ["-m32"] -- todo: do this only for x86 targets
  let watsonPluginCPPObjBuildCfg =
        (def :: CPPObjBuildCfg)
          & field @"target" .~ watsonPluginCPPTargetCfg
          & field @"srcs" .~ watsonPluginCPPSourceCfg
          & field @"includes" .~ watsonPluginCPPIncludeCfg
          & field @"flags" .~ watsonPluginCPPObjBuildFlagCfg
  genCPPObjBuildRules watsonPluginCPPObjBuildCfg

  let watsonPluginCPPObjSourceCfg =
        (def :: CPPObjSourceCfg)
          & field @"fromCPPObjBuild" .~ [watsonPluginCPPObjBuildCfg]
          & field @"fromWorld" .~ ((("sdk"</>"hl2sdk-tf2"</>"lib"</>"mac") </>) <$> ["libtier0.dylib", "libvstdlib.dylib", "tier1_i486.a"])
  let watsonPluginCPPLinkBuildFlagCfg =
        defaultCPPLinkBuildFlagCfg
          & field @"common" <>~ ["-dynamiclib"]

          & field @"common" <>~ ["-m32", "-arch", "i386"] -- todo: do this only for x86 targets
  let watsonPluginCPPLinkBuildCfg =
        (def :: CPPLinkBuildCfg)
          & field @"name" .~ "bin"<.>"dylib"
          & field @"target" .~ watsonPluginCPPTargetCfg
          & field @"objs" .~ watsonPluginCPPObjSourceCfg
          & field @"flags" .~ watsonPluginCPPLinkBuildFlagCfg
  genCPPLinkBuildRules watsonPluginCPPLinkBuildCfg

  let watsonPluginDistDir = dircfg^.field @"dist"</>"watson"
  let watsonPluginBinDir = watsonPluginDistDir</>"bin"
  let watsonPluginBaseDylib = watsonPluginBinDir</>"base"<.>"dylib"
  let watsonPluginVdf = dircfg^.field @"dist"</>"watson"<.>"vdf"

  linkToDistRules (getCPPLinkPrimaryBuildOut watsonPluginCPPLinkBuildCfg) watsonPluginBaseDylib
  linkToDistRules (dircfg^.field @"res"</>"watson.vdf") watsonPluginVdf

  phony watsonPluginDistDir $ do
    need [watsonPluginBaseDylib, watsonPluginVdf]

  want [watsonPluginDistDir]

  return ()

data SDKArch = SDKArch_x86 | SDKArch_x64 deriving (Generic, Show, Read, Eq, Enum)
data SDKPlatforms = SDKPlatforms {
  macArchs :: [SDKArch],
  linuxArchs :: [SDKArch],
  winArchs :: [SDKArch]
} deriving (Generic, Default, Show)
data SDKData = SDKData {
  name :: String,
  folder :: String,
  define :: String,
  code :: Int,
  platform :: SDKPlatforms
} deriving (Generic, Show)

winOnlySDKPlatforms :: SDKPlatforms
winOnlySDKPlatforms = def & field @"winArchs" .~ [SDKArch_x86]

winLinuxSDKPlatforms :: SDKPlatforms
winLinuxSDKPlatforms =
  def
    & field @"winArchs" .~ [SDKArch_x86]
    & field @"linuxArchs" .~ [SDKArch_x86]

winLinuxMacSDKPlatforms :: SDKPlatforms
winLinuxMacSDKPlatforms =
  def
    & field @"winArchs" .~ [SDKArch_x86]
    & field @"linuxArchs" .~ [SDKArch_x86]
    & field @"macArchs" .~ [SDKArch_x86]

csgoSDKPlatforms :: SDKPlatforms
csgoSDKPlatforms =
  def
    & field @"winArchs" .~ [SDKArch_x86]
    & field @"linuxArchs" .~ [SDKArch_x86, SDKArch_x64]
    & field @"macArchs" .~ [SDKArch_x64]

source2SDKPlatforms :: SDKPlatforms
source2SDKPlatforms =
  def
    & field @"winArchs" .~ [SDKArch_x86, SDKArch_x64]
    & field @"linuxArchs" .~ [SDKArch_x64]

sdks :: [SDKData]
sdks = [
    SDKData {name = "ep1",         folder = "episode1",    define = "SE_EPISODEONE",     code = 1,  platform = winLinuxSDKPlatforms},
    SDKData {name = "darkm",       folder = "darkm",       define = "SE_DARKMESSIAH",    code = 2,  platform = winOnlySDKPlatforms},
    SDKData {name = "ep2",         folder = "orangebox",   define = "SE_ORANGEBOX",      code = 3,  platform = winLinuxSDKPlatforms},
    SDKData {name = "bgt",         folder = "bgt",         define = "SE_BLOODYGOODTIME", code = 4,  platform = winOnlySDKPlatforms},
    SDKData {name = "eye",         folder = "eye",         define = "SE_EYE",            code = 5,  platform = winOnlySDKPlatforms},
    SDKData {name = "css",         folder = "css",         define = "SE_CSS",            code = 6,  platform = winLinuxMacSDKPlatforms},
    SDKData {name = "hl2dm",       folder = "hl2dm",       define = "SE_HL2DM",          code = 7,  platform = winLinuxMacSDKPlatforms},
    SDKData {name = "dods",        folder = "dods",        define = "SE_DODS",           code = 8,  platform = winLinuxMacSDKPlatforms},
    SDKData {name = "sdk2013",     folder = "sdk2013",     define = "SE_SDK2013",        code = 9,  platform = winLinuxMacSDKPlatforms},
    SDKData {name = "bms",         folder = "bms",         define = "SE_BMS",            code = 10, platform = winLinuxSDKPlatforms},
    SDKData {name = "tf2",         folder = "tf2",         define = "SE_TF2",            code = 11, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "l4d",         folder = "l4d",         define = "SE_LEFT4DEAD",      code = 12, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "nucleardawn", folder = "nucleardawn", define = "SE_NUCLEARDAWN",    code = 13, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "contagion",   folder = "contagion",   define = "SE_CONTAGION",      code = 14, platform = winOnlySDKPlatforms},
    SDKData {name = "l4d2",        folder = "l4d2",        define = "SE_LEFT4DEAD2",     code = 15, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "swarm",       folder = "swarm",       define = "SE_ALIENSWARM",     code = 16, platform = winOnlySDKPlatforms},
    SDKData {name = "portal2",     folder = "portal2",     define = "SE_PORTAL2",        code = 17, platform = def},
    SDKData {name = "blade",       folder = "blade",       define = "SE_BLADE",          code = 18, platform = winLinuxSDKPlatforms},
    SDKData {name = "insurgency",  folder = "insurgency",  define = "SE_INSURGENCY",     code = 19, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "doi",         folder = "doi",         define = "SE_DOI",            code = 20, platform = winLinuxMacSDKPlatforms},
    SDKData {name = "csgo",        folder = "csgo",        define = "SE_CSGO",           code = 21, platform = csgoSDKPlatforms},
    SDKData {name = "dota",        folder = "dota",        define = "SE_DOTA",           code = 22, platform = source2SDKPlatforms}
  ]

shakeOptions' :: DirCfg -> ShakeOptions
shakeOptions' dc = shakeOptions{
    shakeFiles = bldPath</>"builddb",
    shakeReport = ((bldPath </> "report") </>) <$> [
      "t"<.>"trace", "h"<.>"html"
    ],
    shakeLint = Just LintBasic,
    shakeTimings = True
  }
  where
    bldPath :: FilePath
    bldPath = projectCfg^.field @"dircfg".field @"bld"


data ProjectCfg = ProjectCfg {
  dircfg :: DirCfg,
  clangCompDB :: String
} deriving (Generic, Show)
instance Default ProjectCfg where
  def = ProjectCfg {dircfg = def, clangCompDB = (def :: DirCfg)^.field @"bld" </> "compile_commands.json"}

data DirCfg = DirCfg {
  src :: FilePath,
  dist :: FilePath,
  bld :: FilePath,
  res :: FilePath,
  world:: FilePath
} deriving (Generic, Show)
instance Default DirCfg where
  def = DirCfg {
    src = "src",
    dist = "dist",
    bld = "bld",
    res = "res",
    world = "world"
  }


data CPPTargetCfg = CPPTargetCfg {
  name :: String,
  dircfgOverride :: Maybe DirCfg,

  publicIncludeDirs :: [FilePath],
  publicCPPObjs :: [FilePath],
  publicCPPSources :: [FilePath]
} deriving (Generic, Default, Show)
defaultDircfgForCPPTargetNamed :: String -> DirCfg
defaultDircfgForCPPTargetNamed name =
  foldr (\l cfg-> over l (</> name) cfg) (projectCfg^.field @"dircfg") [field @"src", field @"bld", field @"dist"]

dircfgForCPPTarget :: CPPTargetCfg -> DirCfg
dircfgForCPPTarget cfg =
  case cfg^.field @"dircfgOverride" of
    Just x -> x
    Nothing -> defaultDircfgForCPPTargetNamed $ cfg^.field @"name"

data CPPIncludeListing = CPPIncludeListing {
  fromInTarget :: [FilePath],
  fromOtherTarget :: [CPPTargetCfg],
  fromWorld :: [FilePath],
  fromUnportablePath :: [FilePath]
} deriving (Generic, Default, Show)
cppIncludeListingToPaths :: CPPTargetCfg -> CPPIncludeListing -> [FilePath]
cppIncludeListingToPaths tcfg ilist =
  join $
    [(ilist^.field @"fromUnportablePath")] ++
    [(tdircfg^.field @"src" </>) <$> (ilist^.field @"fromInTarget")] ++
    [(tdircfg^.field @"world" </>) <$> (ilist^.field @"fromWorld")] ++
    (includePathsForTarget <$> (ilist^.field @"fromOtherTarget"))
  where
    tdircfg :: DirCfg
    tdircfg = dircfgForCPPTarget tcfg

    includePathsForTarget :: CPPTargetCfg -> [FilePath]
    includePathsForTarget tcfg =
      let dircfg = dircfgForCPPTarget tcfg
      in (dircfg^.field @"dist" </>) <$> tcfg^.field @"publicIncludeDirs"

data CPPIncludeCfg = CPPIncludeCfg {
  warn :: CPPIncludeListing,
  noWarn :: CPPIncludeListing
} deriving (Generic, Default, Show)


data CPPFlagCfg = CPPFlagCfg {
  common :: [String],
  debug :: [String],
  release :: [String]
} deriving (Generic, Default, Show)

defaultCPPObjectBuildFlagCfg :: CPPFlagCfg
defaultCPPObjectBuildFlagCfg =
  CPPFlagCfg {
    common = defaultCommon,
    debug = ["-O0", "-g", "-glldb"],
    release = ["-Ofast"]
  }
  where
    defaultCommon :: [String]
    defaultCommon =
      let
        warnFlags = [
            "-Weverything", "-Wno-padded", -- "-Wno-float-equal",
            "-Wno-c++98-compat", "-Wno-c++98-c++11-compat", "-Wno-c++98-c++11-compat-pedantic",
            "-Wno-c++98-compat-pedantic", "-Wno-c99-extensions", "-Wno-c++98-c++11-c++14-compat"
          ]
        diagFlags = ["-fcolor-diagnostics"]
      in ["-std=c++1z"] ++ warnFlags ++ diagFlags


data CPPSourceCfg = CPPSourceCfg {
  fromInTarget :: [FilePath],
  fromOtherTarget :: [CPPTargetCfg],
  fromWorld :: [FilePath],
  fromUnportablePath :: [FilePath]
} deriving (Generic, Default, Show)
cppSourceCfgToPatterns :: CPPTargetCfg -> CPPSourceCfg -> [Pattern]
cppSourceCfgToPatterns tcfg cfg =
  let strs = join $
        [(tdircfg^.field @"src"</>) <$> (cfg^.field @"fromInTarget")] ++
        (srcPatternsForTarget <$> (cfg^.field @"fromOtherTarget")) ++
        [(tdircfg^.field @"world"</>) <$> (cfg^.field @"fromWorld")] ++
        [cfg^.field @"fromUnportablePath"]
  in compile <$> strs
  where
    tdircfg :: DirCfg
    tdircfg = dircfgForCPPTarget tcfg

    srcPatternsForTarget :: CPPTargetCfg -> [String]
    srcPatternsForTarget tcfg =
      let dircfg = dircfgForCPPTarget tcfg
      in (dircfg^.field @"dist" </>) <$> tcfg^.field @"publicCPPSources"
cppSourceCfgToPaths :: CPPTargetCfg -> CPPSourceCfg -> IO [FilePath]
cppSourceCfgToPaths tcfg cfg =
  dedup <$> join <$> globDir (cppSourceCfgToPatterns tcfg cfg) "."

data CPPObjBuildCfg = CPPObjBuildCfg {
  target :: CPPTargetCfg,
  dircfgOverride :: Maybe DirCfg,
  srcs :: CPPSourceCfg,

  includes :: CPPIncludeCfg,
  compiler :: String,
  debug :: Bool,
  flags :: CPPFlagCfg
} deriving (Generic, Show)
instance Default CPPObjBuildCfg where
  def = CPPObjBuildCfg {
      target = def,
      dircfgOverride = def,
      srcs = def,

      includes = def,
      compiler = "clang++",
      debug = True,
      flags = defaultCPPObjectBuildFlagCfg
    }

findCPPObjectBuildOuts :: CPPObjBuildCfg -> IO [FilePath]
findCPPObjectBuildOuts cfg = do
  let outFromSrc = \x -> dircfg^.field @"bld"</>dropKnownDirname (dircfg^.field @"src") x-<.>"o"
  (fmap outFromSrc) <$> cppSourceCfgToPaths (cfg^.field @"target") (cfg^.field @"srcs")
  where
    dircfg :: DirCfg
    dircfg = dircfgForCPPObjBuild cfg

genCPPObjBuildRules :: CPPObjBuildCfg -> Rules ()
genCPPObjBuildRules cfg = do
  files <- liftIO $ findCPPObjectBuildOuts cfg

  let depFromOut = \x -> gendDepDir</>dropKnownDirname bldDir x-<.>"dep"
  let outFromDep = \x -> bldDir</>dropKnownDirname gendDepDir x-<.>"o"
  let clangCDBFromOut = \x -> clangCDBDir</>dropKnownDirname bldDir x-<.>"json"
  let outFromClangCDB = \x -> bldDir</>dropKnownDirname clangCDBDir x-<.>"o"

  files |%> \f -> buildFn f (depFromOut f) (clangCDBFromOut f)
  fmap depFromOut files |%> \x -> need [outFromDep x]
  fmap clangCDBFromOut files |%> \x -> need [outFromClangCDB x]

  where
    tcfg :: CPPTargetCfg
    tcfg = cfg^.field @"target"


    dircfg :: DirCfg
    dircfg = dircfgForCPPObjBuild cfg

    bldDir :: FilePath
    bldDir = dircfg^.field @"bld"
    gendDepDir :: FilePath
    gendDepDir = bldDir</>"gendDep"
    clangCDBDir :: FilePath
    clangCDBDir = bldDir</>"clangCDB"

    buildFn :: FilePath -> FilePath -> FilePath -> Action ()
    buildFn out dep clangCDB = do
      liftIO $ forM_ (takeDirectory <$> [out, dep, clangCDB]) (createDirectoryIfMissing True)

      let src = (dircfg^.field @"src")</>dropKnownDirname (dircfg^.field @"bld") out-<.>"cpp"
      need [src]

      let command =
            [cfg^.field @"compiler"] ++
            (cfg^.field @"flags".field @"common") ++
            (if cfg^.field @"debug" then cfg^.field @"flags".field @"debug" else cfg^.field @"flags".field @"release") ++
            (("-isystem"++) <$> cppIncludeListingToPaths tcfg (cfg^.field @"includes".field @"noWarn")) ++
            (("-I"++) <$> cppIncludeListingToPaths tcfg (cfg^.field @"includes".field @"warn")) ++
            ["-o", out]

      cmd_ command "-M" "-MF" [dep] [src]
      needMakefileDependencies dep

      cmd_ command "-MJ" [clangCDB] "-c" [src]
      return ()
dircfgForCPPObjBuild :: CPPObjBuildCfg -> DirCfg
dircfgForCPPObjBuild cfg =
  case cfg^.field @"dircfgOverride" of
    Just x -> x
    Nothing -> over (field @"bld") (</> "cpp_o") (dircfgForCPPTarget $ cfg^.field @"target")


data CPPObjSourceCfg = CPPObjSourceCfg {
  fromInTarget :: [FilePath],
  fromOtherTarget :: [CPPTargetCfg],
  fromWorld :: [FilePath],
  fromUnportablePath :: [FilePath],
  fromCPPObjBuild :: [CPPObjBuildCfg]
} deriving (Generic, Default, Show)
cppObjSourceCfgToPaths :: CPPTargetCfg -> CPPObjSourceCfg -> IO [FilePath]
cppObjSourceCfgToPaths tcfg cfg = do
  cppObjBuildOuts <- mapM findCPPObjectBuildOuts (cfg^.field @"fromCPPObjBuild")
  return . join $
    [(tdircfg^.field @"src"</>) <$> (cfg^.field @"fromInTarget")] ++
    (objPathsForTarget <$> (cfg^.field @"fromOtherTarget")) ++
    [(tdircfg^.field @"world"</>) <$> (cfg^.field @"fromWorld")] ++
    [cfg^.field @"fromUnportablePath"] ++
    cppObjBuildOuts
  where
    tdircfg :: DirCfg
    tdircfg = dircfgForCPPTarget tcfg

    objPathsForTarget :: CPPTargetCfg -> [FilePath]
    objPathsForTarget tcfg =
      let dircfg = dircfgForCPPTarget tcfg
      in (dircfg^.field @"dist" </>) <$> tcfg^.field @"publicCPPObjs"


data CPPLibSearchPathCfg = CPPLibSearchPathCfg {
  fromWorld :: [FilePath],
  fromUnportablePath :: [FilePath]
} deriving (Generic, Default, Show)
cppLibSearchPathCfgToPaths :: CPPTargetCfg -> CPPLibSearchPathCfg -> [FilePath]
cppLibSearchPathCfgToPaths tcfg cfg =
  ((tdircfg^.field @"world" </>) <$> (cfg^.field @"fromWorld")) ++ (cfg^.field @"fromUnportablePath")
  where
    tdircfg :: DirCfg
    tdircfg = dircfgForCPPTarget tcfg
data CPPLostLibCfg = CPPLostLibCfg {
  names :: [String],
  searchPath :: CPPLibSearchPathCfg
} deriving (Generic, Default, Show)

data CPPLinkBuildCfg = CPPLinkBuildCfg {
  name :: String,
  target :: CPPTargetCfg,
  dircfgOverride :: Maybe DirCfg,
  objs :: CPPObjSourceCfg,

  lostLibs :: CPPLostLibCfg,
  linker :: String,
  debug :: Bool,
  flags :: CPPFlagCfg
} deriving (Generic, Show)
instance Default CPPLinkBuildCfg where
  def = CPPLinkBuildCfg {
      name = "dist",
      target = def,
      dircfgOverride = def,
      objs = def,

      lostLibs = def,
      linker = "clang++",
      debug = True,
      flags = defaultCPPLinkBuildFlagCfg
    }

defaultCPPLinkBuildFlagCfg :: CPPFlagCfg
defaultCPPLinkBuildFlagCfg =
  CPPFlagCfg {
    common = [],
    debug = [],
    release = []
  }

genCPPLinkBuildRules :: CPPLinkBuildCfg -> Rules ()
genCPPLinkBuildRules cfg =
  getCPPLinkPrimaryBuildOut cfg %> \out -> do
    srcs <- liftIO $ cppObjSourceCfgToPaths tcfg (cfg^.field @"objs")
    need srcs

    let command =
          [cfg^.field @"linker"] ++
          (cfg^.field @"flags".field @"common") ++
          (if cfg^.field @"debug" then cfg^.field @"flags".field @"debug" else cfg^.field @"flags".field @"release") ++
          (("-L"++) <$> (cppLibSearchPathCfgToPaths tcfg $ cfg^.field @"lostLibs".field @"searchPath")) ++
          (("-l"++) <$> (cfg^.field @"lostLibs".field @"names")) ++
          ["-o", out]

    cmd_ command srcs
    return ()
  where
    tcfg :: CPPTargetCfg
    tcfg = cfg^.field @"target"

    dircfg :: DirCfg
    dircfg = dircfgForCPPLinkBuild cfg
dircfgForCPPLinkBuild :: CPPLinkBuildCfg -> DirCfg
dircfgForCPPLinkBuild cfg =
  case cfg^.field @"dircfgOverride" of
    Just x -> x
    Nothing -> over (field @"bld") (</> "cpp_bin") (dircfgForCPPTarget $ cfg^.field @"target")

getCPPLinkPrimaryBuildOut :: CPPLinkBuildCfg -> FilePath
getCPPLinkPrimaryBuildOut cfg = (dircfgForCPPLinkBuild cfg)^.field @"bld"</>cfg^.field @"name"

getCPPLinkBuildOuts :: CPPLinkBuildCfg -> [FilePath]
getCPPLinkBuildOuts cfg =
  [getCPPLinkPrimaryBuildOut cfg]
  where
    dircfg :: DirCfg
    dircfg = dircfgForCPPLinkBuild cfg

linkToDistRules :: FilePath -> FilePath -> Rules ()
linkToDistRules inp out =
  out %> \_ -> do
    cwd <- liftIO $ getCurrentDirectory
    need [inp]
    cmd_ "ln" "-sf" [cwd</>inp] out

dropKnownDirname :: FilePath -> FilePath -> FilePath
dropKnownDirname base x = joinPath $ dropDir (splitPath base) (splitPath x)
  where
    dropDir :: [FilePath] -> [FilePath] -> [FilePath]
    dropDir [] xs = xs
    dropDir _ [] = []
    dropDir (b:bs) (x:xs) =
      if x == "." || x == "./" then
        dropDir (b:bs) xs
      else if b == "." || b == "./" then
        dropDir bs (x:xs)
      else
        let diff = x \\ b
        in if diff == "" || diff == "/" then
            dropDir bs xs
          else
            error $ "Could not drop '"++(joinPath (b:bs))++"' from '"++(joinPath (x:xs))++"'\n" ++ show (b:bs) ++ " " ++ show (x:xs)

dedup :: (Eq a, Ord a) => [a] -> [a]
dedup = map head . group . sort
