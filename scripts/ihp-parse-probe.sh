#!/usr/bin/env bash
# Run the IHC parse probe over a set of IHP source files.
# Usage:
#   scripts/ihp-parse-probe.sh [IHP_ROOT]          # probe ~50 representative files
#   scripts/ihp-parse-probe.sh --all [IHP_ROOT]    # probe all .hs files (slow)
#
# Requires: cabal build exe:ihc-parse-probe (already in ihc.cabal)
set -euo pipefail

IHP_ROOT="${1:-/Users/marc/digitallyinduced/ihp}"
PROBE="$(cabal list-bin exe:ihc-parse-probe 2>/dev/null || \
          find dist-newstyle -name ihc-parse-probe -type f | head -1)"

if [[ -z "$PROBE" ]]; then
    echo "ihc-parse-probe binary not found. Run: cabal build exe:ihc-parse-probe"
    exit 1
fi

if [[ "${1:-}" == "--all" ]]; then
    IHP_ROOT="${2:-/Users/marc/digitallyinduced/ihp}"
    find "$IHP_ROOT" -name '*.hs' \
        -not -path '*/dist-newstyle/*' \
        -not -path '*/.git/*' \
    | "$PROBE"
else
    # Representative 50-file sample
    cat <<FILES | "$PROBE"
$IHP_ROOT/ihp/IHP/ModelSupport.hs
$IHP_ROOT/ihp/IHP/QueryBuilder.hs
$IHP_ROOT/ihp/IHP/HaskellSupport.hs
$IHP_ROOT/ihp/IHP/ControllerSupport.hs
$IHP_ROOT/ihp/IHP/RouterSupport.hs
$IHP_ROOT/ihp/IHP/ViewSupport.hs
$IHP_ROOT/ihp/IHP/FrameworkConfig.hs
$IHP_ROOT/ihp/IHP/Controller/Param.hs
$IHP_ROOT/ihp/IHP/Controller/Context.hs
$IHP_ROOT/ihp/IHP/Controller/Render.hs
$IHP_ROOT/ihp/IHP/Controller/Redirect.hs
$IHP_ROOT/ihp/IHP/Controller/Layout.hs
$IHP_ROOT/ihp/IHP/Controller/Cookie.hs
$IHP_ROOT/ihp/IHP/Controller/Session.hs
$IHP_ROOT/ihp/IHP/Controller/AccessDenied.hs
$IHP_ROOT/ihp/IHP/Controller/FileUpload.hs
$IHP_ROOT/ihp/IHP/ValidationSupport/Types.hs
$IHP_ROOT/ihp/IHP/ValidationSupport/ValidateField.hs
$IHP_ROOT/ihp/IHP/Fetch.hs
$IHP_ROOT/ihp/IHP/FetchRelated.hs
$IHP_ROOT/ihp/IHP/FetchPipelined.hs
$IHP_ROOT/ihp/IHP/FlashMessages.hs
$IHP_ROOT/ihp/IHP/ErrorController.hs
$IHP_ROOT/ihp/IHP/ControllerPrelude.hs
$IHP_ROOT/ihp/IHP/ActionType.hs
$IHP_ROOT/ihp/IHP/Job/Queue.hs
$IHP_ROOT/ihp/IHP/Job/Runner.hs
$IHP_ROOT/ihp/IHP/Job/Types.hs
$IHP_ROOT/ihp/IHP/View/Form.hs
$IHP_ROOT/ihp/IHP/View/TimeAgo.hs
$IHP_ROOT/ihp/IHP/View/CSSFramework.hs
$IHP_ROOT/ihp/IHP/Pagination/ControllerFunctions.hs
$IHP_ROOT/ihp/IHP/Pagination/ViewFunctions.hs
$IHP_ROOT/ihp/IHP/Pagination/Types.hs
$IHP_ROOT/ihp/IHP/AuthSupport/Authentication.hs
$IHP_ROOT/ihp/IHP/AuthSupport/Controller/Sessions.hs
$IHP_ROOT/ihp/IHP/AutoRefresh.hs
$IHP_ROOT/ihp/IHP/AutoRefresh/Types.hs
$IHP_ROOT/ihp-hsx/blaze/IHP/HSX/QQ.hs
$IHP_ROOT/ihp-hsx/blaze/IHP/HSX/Attribute.hs
$IHP_ROOT/ihp-hsx/parser/IHP/HSX/Parser.hs
$IHP_ROOT/ihp-hsx/parser/IHP/HSX/HaskellParser.hs
$IHP_ROOT/ihp-ide/IHP/IDE/Types.hs
$IHP_ROOT/ihp-ide/IHP/IDE/SchemaDesigner/Types.hs
$IHP_ROOT/ihp-ide/IHP/IDE/SchemaDesigner/Controller/Columns.hs
$IHP_ROOT/ihp-ide/IHP/IDE/ToolServer/Types.hs
$IHP_ROOT/ihp-ssc/IHP/ServerSideComponent/Types.hs
$IHP_ROOT/ihp-ssc/IHP/ServerSideComponent/HtmlDiff.hs
$IHP_ROOT/integration-test/Web/Controller/Posts.hs
$IHP_ROOT/integration-test/Web/Routes.hs
FILES
fi
