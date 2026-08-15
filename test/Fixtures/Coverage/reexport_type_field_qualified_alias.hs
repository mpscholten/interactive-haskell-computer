-- Record field accessor through a qualified alias of a gateway that
-- re-exports T(..).  Warp hello leftover: import qualified
-- Network.HTTP.Types as H then H.statusCode H.status200 was unbound
-- because Status(..) on the facade has no local field registry.
-- Custom ADT so this is not a name list of H.statusCode.
import qualified Modules.ReexportTypeFields.Gateway as H

main :: IO ()
main = print (H.hType H.defaultHints)
