#' Generic function 
#' @name createSimulationTable
#' @aliases createSimulationTable
#' @title Generic function 
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{createSimulationTable.ezsim}} 
createSimulationTable <-
function(x,...){
    UseMethod("createSimulationTable")
}

#' Generic function
#' @name generate
#' @aliases generate
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{generate.parameterDef}}
generate <-
function(x,...){
    UseMethod("generate")
}

#' Generic function
#' @name setBanker
#' @aliases setBanker
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{setBanker.parameterDef}}
setBanker <-
function(x,...){
    UseMethod("setBanker")
}

#' Generic function
#' @name run
#' @aliases run
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{run.ezsim}}

run <-
function(x,...){
    UseMethod("run")
}
#' Generic function
#' @name getScalarsName
#' @aliases getScalarsName
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{getScalarsName.ezsim}}, \code{\link{getScalarsName.summary.ezsim}}

getSelectionName <-
function(x,...){
    UseMethod("getSelectionName")
}
#' Generic function
#' @name setBanker
#' @aliases setBanker
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{setBanker.parameterDef}}

setBanker <-
function(x,...){
    UseMethod("setBanker")
}

#' Generic function
#' @name setSelection
#' @aliases setSelection
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal
#' @seealso \code{\link{setSelection.parameterDef}}
setSelection <-
function(x,...){
    UseMethod("setSelection")
}

#' Generic function
#' @name test
#' @aliases test
#' @title Generic function
#' @param x Object
#' @param \dots Further arguments
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @seealso \code{\link{test.ezsim}}
test <-
function(x,...){
    UseMethod("test")
}

