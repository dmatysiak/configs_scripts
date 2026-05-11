; inherits: forester

(comment) @comment

[
 "\\"
 "("
 ")"
 "{"
 "}"
 "["
 "]"
] @punctuation.bracket

(p "p" @function.builtin)
(li "li" @markup.list)
(ul "ul"  @markup.list)
(ol "ol"  @markup.list)
(em "em"  @function.builtin)
(strong "strong" @function.builtin)
(code "code" @function.builtin)

(tag "tag" @field)
(author "author" @field)
(contributor "contributor" @field)
(title "title" @field)

(title "title" @text.title)
(title (_) @text.title)
(author author: (_) @markup.heading.url)

(tex "tex" @function.builtin)

(subtree "subtree" @keyword.function)

(transclude "transclude" @include)
(transclude address: (_) @markup.link.url)

(def "def" @keyword)
(let "let" @keyword)
(object "object" @constant)
(object self: (_) @keyword)
(method_decl key: (_) @method)
(patch "patch" @text.diff.add)
(patch object: (_) @constant)

(markdown_link label: (_) @label)
(markdown_link dest: (_) @text.uri)
(unlabeled_link (external_link) @text.uri)

(scope "scope" @namespace)
(put "put" @variable.parameter)

(query_tree "query" @keyword)

(import "import" @include)
(export "export" @include)
(transclude "transclude" @include)
