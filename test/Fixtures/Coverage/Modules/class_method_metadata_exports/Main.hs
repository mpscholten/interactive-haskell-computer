import OwnerAll (markerAll)
import OwnerExplicit (markerExplicit)
import OwnerHidden (markerHidden)
import OwnerImportHiding (markerImportHiding)
import OwnerAmbiguous (markerAmbiguous)
main = markerAll + markerExplicit + markerHidden + markerImportHiding + markerAmbiguous
