trimNames <- function(string){
  regexprOutput <- regexpr(pattern = "AB.*MSMS", string)
  match_length <- unname(attributes(regexprOutput)["match.length"]$match.length)
  substring(string, first = regexprOutput[1], last = regexprOutput + match_length - 1)
}
