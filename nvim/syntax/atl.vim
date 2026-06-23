if exists("b:current_syntax")
    finish
endif

syntax keyword atlKeyword fn match with in record open enum Module let optional list set field-access enum-cons enum-tail enum-head as annotate opened
syntax keyword atlKeyword Definition nextgroup=atlDefinition skipwhite
syntax keyword atlKeyword UnitTest nextgroup=atlUnitTest skipwhite
syntax keyword atlKeyword ValidationTest nextgroup=atlValidationTest skipwhite
highlight link atlKeyword Keyword

syntax keyword atlInclude Require
highlight link atlInclude Include

syntax keyword atlType prop type string
highlight link atlType Type

syntax match atlDefinition display contained @\%([A-Za-z][A-Za-z0-9-]*\|'\%(\\'\|[^']\)*'\)@
highlight link atlDefinition Function

syntax match atlUnitTest display contained @\%([A-Za-z][A-Za-z0-9-]*\|'\%(\\'\|[^']\)*'\)@
highlight link atlUnitTest Function

syntax match atlValidationTest display contained @\%([A-Za-z][A-Za-z0-9-]*\|'\%(\\'\|[^']\)*'\)@
highlight link atlValidationTest Function

syntax match atlOperator ":"
syntax match atlOperator ":="
syntax match atlOperator "=>"
syntax match atlOperator "->"
highlight link atlOperator Operator

syntax keyword atlFunction annotate
highlight link atlFunction Function

syntax match atlEscape contained "\\."
syntax region atlString start='"' end='"' contains=atlEscape
highlight link atlString String
highlight link atlEscape SpecialChar

syntax region atlIdentifier start="'" end="'"
highlight link atlIdentifier Identifier

syntax match atlComment "//.*$"
syntax region atlComment start=/\/\*/ end=/\*\//
highlight link atlComment Comment

let b:current_syntax = "atl"
