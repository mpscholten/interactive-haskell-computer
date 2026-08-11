import OwnerFacade (markerFacade)
import OwnerAlias (markerAlias)
import OwnerAmbiguous (markerAmbiguousFacade)
import OwnerHidden (markerHidden)
import OwnerClassOnly (markerClassOnly)

main = markerFacade + markerAlias + markerAmbiguousFacade + markerHidden + markerClassOnly
