import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    buildHook = buildCPlusPlus
  }

buildCPlusPlus :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildCPlusPlus pkg buildInfo hooks flags = do
  -- library
  progc "clang++" flags
    ["-stdlib=libc++", "-o", "cbits/hsprotobuf.o", "-c", "cbits/hsprotobuf.cc"]
  case executables pkg of
    [Executable {exeName = "protobuf-native-test"}] -> do
      progc "clang++" flags
        ["-stdlib=libc++", "-o", "tests/person.pb.o", "-c", "tests/person.pb.cc"]
    [] -> return ()
    e -> error $ "unknown build target: " ++ show e
  buildHook simpleUserHooks pkg buildInfo hooks flags

progc prog flags cc_flags = do
  let verb = fromFlag (buildVerbosity flags)
  clang <- findProgramLocation verb prog
  let clang' = case clang of
                Just x -> x
                Nothing -> error $ prog ++ " not on path"
  runProgram verb (simpleConfiguredProgram prog (FoundOnSystem clang')) cc_flags
