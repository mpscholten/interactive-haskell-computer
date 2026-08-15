-- popCallStack EmptyCallStack is identity: IHC never synthesises
-- HasCallStack frames, so withFrozenCallStack always pops empty.
-- Push / Freeze still go through source.
import GHC.Stack (emptyCallStack, popCallStack)

main :: IO ()
main = print (popCallStack emptyCallStack)
