
import Moonbase


xterm :: Terminal
xterm (Just args') = return $ "term: " ++ args'
xterm Nothing     = return "plain term"

main :: IO ()
main = moonbase xterm $ return ()
