structure XML =
struct

  structure S = TextIO.StreamIO

  type attr = string*string
  datatype tag = Eof 
                    | EmptyTag of string*(attr list)
                    | StartTag of string*(attr list)
                    | EndTag 
                    | Other

  datatype tree = Node of string*(attr list)*(tree list)

  val showTagCount : bool ref = ref(false)
  val tagCount : int ref = ref(0)

  fun getNodeAndAttrs(body : string) : string*(attr list) =
  let
    fun kvsplit (s : string) : attr =
    let
      val (k, v') = Substring.splitl (fn c => c <> #"=") (Substring.full s)
      val v = Substring.trimr 1 (Substring.triml 2 v')
    in
      (Substring.string k, Substring.string v)
    end

    fun kvpairs(s : Substring.substring) : attr list =
    let
      (*
      val _ = print (concat ["kvpairs ", Substring.string s, "\n"])
      *)
      val s' = Substring.dropl Char.isSpace s
    in
      if Substring.size s' = 0 then []
      else
      let
        val (key, rest) = Substring.splitl (fn c => c <> #"=") s'
        (*  rest = "#...# *)
        val rest = Substring.slice(rest, 2, NONE)
        val (value, rest) = Substring.splitl (fn c => c <> #"\"") rest
        (*  rest = "... *)
        val rest = Substring.slice(rest, 1, NONE)
      in
        (Substring.string key, Substring.string value) :: (kvpairs rest)
      end
    end

    val bodyS = Substring.full body
    (*
    val nodeType :: kvs = String.tokens Char.isSpace body
    val attrs = map kvsplit kvs
    *)
    val (nodeTypeS, restS) = Substring.splitl (Char.isAlphaNum) bodyS
    val attrs = kvpairs restS
  in
    (Substring.string nodeTypeS, attrs)
  end

  fun nextTag (ins : S.instream) : tag*S.instream =
  let
    exception NoTag
    exception BadXML

    (*  tagStart(count, ins) = (n, ins'), where
    *     ins = <c_0, c_1,...>
    *     c_j = #"<", and c_i <> #"<" for i < j.
    *     n = count + j
    *     ins' = <c_{j+1}, c_{j+2},...>
    *)
    fun tagStart(count : int, ins : S.instream) : int*S.instream =
      case S.input1 ins of
           NONE => raise NoTag
         | SOME(c, ins) =>
             if c = #"<" then (count, ins)
             else tagStart(count+1, ins)

    (*  tagEnd(count, ins) = (n, ins'), where
    *     ins = <c_0, c_1,...>
    *     c_j = #">", and c_i <> #">" for i < j.
    *     n = count + j
    *     ins' = <c_{j+1}, c_{j+2},...>
    *)
    fun tagEnd(count : int, ins : S.instream) : int*S.instream =
      case S.input1 ins of
           NONE => raise BadXML
         | SOME(c, ins) =>
             if c = #">" then (count, ins)
             else tagEnd(count+1, ins)

    (*  tagBounds ins = (j, k, ins'), where
    *     ins = <c_0, c_1,...>
    *     c_j = #"<", c_k = #">", c_i <> #"<" for 0<=i<j,
    *                             c_i <> #">" for j<=i<k.
    *     ins' = <c_{k+1}, c_{k+2},...>
    *)
    fun tagBounds(ins : S.instream) : int*int*S.instream =
    let
      val (n0, ins') = tagStart(0, ins)
      val (n1, ins'') = tagEnd(n0+1, ins')
    in
      (n0, n1, ins'')
    end

    (*  extractTag ins = (t, ins'), where
    *     t = the next tag read from ins
    *     ins' = an input stream positioned just after the ">" of the next
    *       tag read from ins.
    *)
    fun extractTag (ins : S.instream) : tag*S.instream =
    let
      val (n0, n1, _) = tagBounds ins
      (*
      val _ = print (concat ["Bounds = ", Int.toString n0, ", ", 
        Int.toString n1, "\n"])
      val _ = print (concat ["fullTag = ", fullTag, "\n"])
      *)
      val (fullTag, ins') = S.inputN (ins, n1+1)
      val fullTag = String.extract (fullTag, n0, NONE)
      val fullTagLen = String.size fullTag
    in
      case String.sub(fullTag, 1) of
           #"?" => (Other, ins')
         | #"/" => (EndTag, ins')
         | _ =>
             case String.sub(fullTag, fullTagLen - 2) of
                  #"/" =>
                  let
                    val bodyLen = fullTagLen - 3
                    val nodeBody = String.substring(fullTag, 1, bodyLen)
                    val (nodeType, attrs) = getNodeAndAttrs(nodeBody)
                  in
                    (EmptyTag(nodeType, attrs), ins')
                  end
                | _ =>
                  let
                    val bodyLen = fullTagLen - 2
                    val nodeBody = String.substring(fullTag, 1, bodyLen)
                    val (nodeType, attrs) = getNodeAndAttrs(nodeBody)
                  in
                    (StartTag(nodeType, attrs), ins')
                  end
    end

    val () = if !showTagCount 
             then 
               (
                 tagCount := !tagCount + 1 ;
                 if !tagCount mod 10000 = 0 
                 then print (concat [Int.toString (!tagCount), "..."])
                 else ()
               )
             else ()
  in
    extractTag ins
    handle NoTag => (if !showTagCount then print "\n" else () ; (Eof, ins))
  end

  (*  parseNodes ins = ts, where ts is the sequence of nodes that
  *   starts at the first character of ins.
  *)
  and parseNodes(ins : S.instream) : (tree list)*(S.instream) =
  let
    (* val () = takeTimer(Parse) *)
  in
    case nextTag ins of
         (Eof, ins) => ([], ins)
       | (Other, ins) => parseNodes ins
       | (EndTag, ins) => ([], ins)
       | (StartTag(nodeType, attrs), ins) =>
         let
           val (children, ins) = parseNodes ins
           val (rest, ins) = parseNodes ins
         in
           (Node(nodeType, attrs, children) :: rest, ins)
         end
       | (EmptyTag(nodeType, attrs), ins) =>
         let
           val (rest, ins) = parseNodes ins
         in
           (Node(nodeType, attrs, []) :: rest, ins)
         end
  end

  fun parseInstream(ins : S.instream) : tree list =
  let
    fun skipLeading (ins : S.instream) : S.instream option =
      case S.inputLine ins of
           NONE => NONE
         | SOME(line, ins') =>
             if String.isPrefix "<?" line then skipLeading ins'
             else SOME(ins)
  in
    case skipLeading ins of
         NONE => []
       | SOME(ins) => #1(parseNodes ins)
  end

  fun parseFile(filename : string) : tree list =
    parseInstream (TextIO.getInstream (TextIO.openIn filename))

  fun domSize(ts : tree list) : int =
    case ts of
         [] => 0
       | t :: ts => treeSize t + domSize ts
  and treeSize(t : tree) : int =
    case t of
         Node(_, _, ts) => 1 + (domSize ts)

end

structure XMLTest =
struct

  fun xmlTest(arg0 : string, filename :: argv : string list) : int =
  let
    val _ = XML.parseFile(filename)
  in
    0
  end

  fun parseFile (arg0 : string, filename :: argv : string list) : int =
  let
    fun indent (s : string, ind : int) : string =
      case ind of
           0 => s
         | _ => " " ^ (indent (s, ind-1))

    fun attrToString ((k, v) : XML.attr) : string =
      String.concat [k, "=", v]

    fun toStringInd (ind : int) (XML.Node(elt, attrs, children) : XML.tree) : string =
      String.concatWith "\n"
        [String.concat [indent(elt, ind), ListFormat.listToString attrToString
        attrs], toStringsInd (ind+2) children]
    and toStringsInd (ind : int) (ts : XML.tree list) : string =
      ListFormat.fmt { init="", sep="\n", final="", fmt=(toStringInd ind) } ts

    val toString = toStringsInd 0

    val xmlfile :: args = CommandLine.arguments()
  in
    (print (toString (XML.parseFile(xmlfile))) ; 0)
  end

end
