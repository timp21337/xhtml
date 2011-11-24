\documentclass [12pt, a4paper, twoside, titlepage] {article}

\usepackage{fullpage}
\usepackage{moreverb}
\usepackage{alltt}
\usepackage{boxedminipage}
\usepackage{fancyhdr}
\usepackage{lastSequpage}
\usepackage{hyperref}
\usepackage{listings}

\newcommand*{\defined}{\textbf}

%include lhs2TeX.sty
%include lhs2TeX.fmt

\pagestyle{fancy}
\cfoot{\thepage\ of \pageref{lastSequPage}}
\headheight 20pt
\headsep 10pt

\begin{document}


\title{XHTML representation}

\author{Tim Pizey
\\@Tim.Pizey@@gmail.com@
\\
\\Page count \pageref{lastSequPage}
}

\maketitle
\clearpage
[Page intentionally left blank]
\clearpage



\section{Part I}
\subsubsection{Setup}

This section describes how to set the project up and what output to expect.

Load this whole file into the Glasgow Haskell Compiler Interactive interface (GHCi).  

Warning level is set to \emph{all}:

> {-# OPTIONS -Wall #-}


> module XHTML (main) where 
> import Control.Applicative
> import Control.Monad

\subsubsection{Testing}

Rather than pasting the results of functions into the body of the text 
equality assertions are used. These are run when the 
file is compiled and executed (a much lighter version of \cite{hunit}). 

> type Assertion = Bool

> assertEqual :: (Eq a) => a -> a -> Bool
> assertEqual expected actual = expected == actual


> tests :: [Assertion]
> tests = [test8, test10, test11, test12, test13, test14, test15, 
>          test16, test17, test18, test19, 
>          test23, test24, test25, test26, test27, test28, test29, 
>          test30, test31, test32, test33, test34, test35, test36, 
>          test37, test38, test39, test40, test41, test42]
> allWell :: Bool
> allWell = and tests

> main :: IO ()
> main = if allWell then do output >> putStr ("OK\n")   
>        else putStr ((show tests) ++ "\n")  

To test all  functions execute \emph{main}:
\begin{verbatim}
*FPR> main
OK
\end{verbatim}

\clearpage

> data Sequ a = Empty | Single a | Cat (Sequ a) (Sequ a) 
>   deriving (Show, Eq, Ord)

Two leaf trees are appended when the lists of their leaves 
are concatSequenated.         

> (+++) :: Sequ a -> Sequ a -> Sequ a
> (+++) Empty y = y
> (+++) x Empty = x
> (+++) x y  = Cat (x) (y)

> -- A pretty printer for Sequ
> pprint :: (Show a) => Sequ a -> String
> pprint Empty         = "E"
> pprint (Single x)    = show x
> pprint (Cat (l) (r)) = "(" ++ pprint l ++ ","
>   ++ pprint r ++ ")"


> test8 :: Assertion
> test8 = assertEqual
>   [
>     "((((2,3),5),7),11)",
>     "(((2,3),5),(7,11))",
>     "(((2,3),(5,7)),11)",
>     "((2,3),((5,7),11))",
>     "((2,3),(5,(7,11)))",
>     "(((2,(3,5)),7),11)",
>     "((2,(3,5)),(7,11))",
>     "((2,((3,5),7)),11)",
>     "(2,(((3,5),7),11))",
>     "(2,((3,5),(7,11)))",
>     "((2,(3,(5,7))),11)",
>     "(2,((3,(5,7)),11))",
>     "(2,(3,((5,7),11)))",
>     "(2,(3,(5,(7,11))))"]
>  (map pprint firstFivePrimeLeafTrees)


> reverseSequ :: Sequ a -> Sequ a
> reverseSequ Empty      = Empty
> reverseSequ (Single a) = (Single a)
> reverseSequ (Cat l r)  = (Cat (reverseSequ r) (reverseSequ l))
>
> test10 :: Assertion
> test10 = assertEqual 
>              (Cat (Single 'b') (Single 'a')) 
>              (reverseSequ (Cat (Single 'a') (Single 'b')))


> lastSequ :: Sequ a -> a
> lastSequ Empty      = undefined
> lastSequ (Single a) = a
> lastSequ (Cat _ r)  = lastSequ r
>
> test11 :: Assertion
> test11 = assertEqual 'g' (lastSequ (sequ "Dog"))
>

> maybelastSequ :: Sequ a -> Maybe a
> maybelastSequ Empty      = Nothing
> maybelastSequ (Single x) = Just x
> maybelastSequ (Cat _ r)  = maybelastSequ (r)
>
> test12 ::Assertion
> test12 = assertEqual
>            Nothing
>            (maybelastSequ (Cat (Single '0') Empty))

> lengthSequ ::  Sequ a -> Integer
> lengthSequ Empty = 0
> lengthSequ (Single _) = 1
> lengthSequ (Cat l r) = (lengthSequ l) + (lengthSequ r) 
>
> test13 :: Assertion
> test13 = assertEqual 2 (lengthSequ (Cat (Cat (Single 'a') (Single 'b')) Empty))

> mapSeq :: (a -> b) -> (Sequ a -> Sequ b)
> mapSeq _ Empty      = Empty
> mapSeq f (Single a) = (Single (f  a))
> mapSeq f (Cat l r)  = (Cat (mapSeq f l) (mapSeq f r))
>
> test14 :: Assertion
> test14 = assertEqual (Cat (Single (2::Int)) (Single (3::Int)))
>          (mapSeq ((+)1) (Cat (Single (1::Int)) (Single (2::Int)))) 

\subsubsection{concatSequ \emph{Sequ}}

> concatSequ :: Sequ (Sequ a) -> Sequ a
> concatSequ Empty            = Empty
> concatSequ (Single a)       = a
> concatSequ (Cat l r )       = (concatSequ l) +++ (concatSequ r)

> test15 :: Assertion
> test15 = assertEqual
>     (Cat (Cat (Single '1')(Single '2'))(Cat (Single '3')(Single '4')))
>     (concatSequ 
>        (Cat (Single (Cat (Single '1')(Single '2')))
>        (Single (Cat (Single '3') (Single '4'))))
>     )

> concatMapSequ :: (a -> Sequ b) -> (Sequ a -> Sequ b)
> concatMapSequ _ Empty            = Empty
> concatMapSequ f (Single a)       = f a
> concatMapSequ f (Cat l r )       = (concatMapSequ f l) +++ (concatMapSequ f r)
>
> test16 :: Assertion
> test16 = assertEqual 
>     (Cat (Cat (Single 'a') (Single 'b')) (Cat (Single 'c') (Single 'd')))
>     (concatMapSequ (sequ) (Cat (Single "ab") (Single "cd"))) 

\subsubsection{\emph{Sequ} as Functor}

\emph{Sequ} can be made an instance of \emph{Functor} \cite{classes}.

> instance Functor Sequ where
>  fmap _ Empty            = Empty
>  fmap f (Single x)       = Single (f x)
>  fmap f (Cat l r)        = Cat (fmap f l) (fmap f r)
>
> test17 :: Assertion
> test17 = assertEqual (Cat (Single "f-a") (Single "f-b")) 
>              (fmap ((++) "f-") (Cat (Single "a") (Single "b")))

\subsubsection{\emph{Sequ} as Applicative}

> instance Applicative Sequ where
>  pure  = return
>  (<*>) = ap
>
> test18 :: Assertion
> test18 = assertEqual (Cat (Single "f-a") (Single "f-b"))
>     (pure ((++) "f-") <*> (Cat (Single "a") (Single "b")) )

\subsubsection{\emph{Sequ} as Monad}

> instance Monad Sequ where
>  return = Single
>
>  Empty     >>= _   = Empty
>  Single x  >>= f   = f x
>  Cat l r   >>= f   = Cat (l >>= f) (r >>= f)
>
>  fail _ = Empty
>
> test19 :: Assertion
> test19 = assertEqual (Cat (Single "f-a") (Single "f-b"))
>     ((Cat (Single "a") (Single "b")) >>= \x -> return ("f-" ++ x))

> consume :: (a -> b) -> (b -> b -> b) -> b -> Sequ a -> b
> consume _ _    deflt Empty       = deflt
> consume f _    _     (Single x)  = (f x)
> consume f conj deflt (Cat l r)   = conj (consume f conj deflt l) 
>                                         (consume f conj deflt r) 

> test23 :: Assertion 
> test23 = assertEqual (9::Int)
>           (consume (+1) (+) (0) (Cat (Cat(Single 1) (Single 2)) (Single 3))) 

> test24 :: Assertion
> test24 = assertEqual "123" 
>            (consume show (++) "" (Cat (Cat(Single (1::Int)) (Single 2)) (Single 3)))


> reverseSequC :: Sequ a -> Sequ a
> reverseSequC = consume rewrap swap Empty where 
>         rewrap :: a -> Sequ a 
>         rewrap x = (Single x)
>         swap :: Sequ a -> Sequ a -> Sequ a
>         swap l r = Cat r l
>
> test25 :: Assertion
> test25 = assertEqual 
>              (reverseSequ (Cat (Single 'a') (Single 'b')))
>              (reverseSequC (Cat (Single 'a') (Single 'b')))
>

\textbf{ lastSequ \emph{Sequ}}

> lastSequC :: Sequ a -> a
> lastSequC = consume id conj undefined where 
>     conj :: a -> a -> a
>     conj _ r = r
>
> test26 :: Assertion
> test26 = assertEqual 
>          (lastSequ (sequ "Dog")) 
>          (lastSequC (sequ "Dog")) 

> maybelastSequC :: Sequ a -> Maybe a
> maybelastSequC = consume f conj Nothing where 
>       f :: a -> Maybe a
>       f x = Just x
>       conj :: Maybe a -> Maybe a -> Maybe a
>       conj _ r = r
>
> test27 ::Assertion
> test27 = assertEqual
>            (maybelastSequ (Cat (Single '0') Empty))
>            (maybelastSequC (Cat (Single '0') Empty))
>
> lengthSequC ::  Sequ a -> Integer
> lengthSequC = consume (\_ -> 1) (+) 0
>
> test28 :: Assertion
> test28 = assertEqual 
>              (lengthSequ (Cat (Cat (Single 'a') (Single 'b')) Empty))
>              (lengthSequC (Cat (Cat (Single 'a') (Single 'b')) Empty))

> mapSeqC :: (a -> b) -> (Sequ a -> Sequ b)
> mapSeqC f = consume (fwrap f) Cat Empty where
>       fwrap :: (a -> b) -> a -> Sequ b
>       fwrap fn x = (Single $ fn  x)
>
> test29 :: Assertion
> test29 = assertEqual 
>          (mapSeq ((+)1) (Cat (Single (1::Int)) (Single (2::Int)))) 
>          (mapSeqC ((+)1) (Cat (Single (1::Int)) (Single (2::Int)))) 

> concatSequC :: Sequ (Sequ a) -> Sequ a
> concatSequC = consume id (+++) Empty 
>
> test30 :: Assertion
> test30 = assertEqual
>     (concatSequ 
>       (Cat (Single (Cat (Single '1')(Single '2')))
>       (Single (Cat (Single '3') (Single '4'))))
>     )
>     (concatSequC 
>       (Cat (Single (Cat (Single '1') (Single '2')))
>       (Single (Cat (Single '3') (Single '4'))))
>     )

> concatMapSequC :: (a -> Sequ b) -> (Sequ a -> Sequ b)
> concatMapSequC f = consume f (+++) Empty 
>
> test31 :: Assertion
> test31 = assertEqual 
>     (concatMapSequ (sequ) (Cat (Single "ab") (Single "cd"))) 
>     (concatMapSequC (sequ) (Cat (Single "ab") (Single "cd"))) 

> toList :: Sequ a -> [a]
> toList Empty = []
> toList (Single x) = [x]
> toList (Cat (l) (r)) = (toList l) ++ (toList r)
>
> test32 :: Assertion
> test32 = assertEqual 
>   ["2","3","5","7","11"]
>   (toList 
>     (Cat (Cat (Cat (Single "2") (Single "3")) (Single "5")) (Cat (Single "7") (Single "11")))
>   )


> leafTrees ::  [a]  -> [Sequ a]
> leafTrees a = leafTrees' a [Empty]
>
> leafTrees' ::  [a] -> [Sequ a] -> [Sequ a]
> leafTrees' [] soFar = soFar
> leafTrees' (x:xs) soFar =
>   leafTrees' xs (concatMap (addLeaf x) soFar)
> 
> addLeaf :: a -> Sequ a -> [Sequ a]
> addLeaf x Empty = [Single x]
> addLeaf y (Single x) = [Cat (Single x) (Single y)]
> addLeaf x (Cat (l) (r)) = 
>   [Cat (Cat (l) (r)) (Single x)]
>   ++
>   (map (Cat (l)) (addLeaf x r)) 
>
> firstFivePrimeLeafTrees :: [Sequ Integer]
> firstFivePrimeLeafTrees = leafTrees [2,3,5,7,11]

> test33 :: Assertion
> test33 = assertEqual 
>   [
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11],
>     [2,3,5,7,11]
>   ]
>   (map toList firstFivePrimeLeafTrees)

> rsequ :: [a] -> Sequ a
> rsequ [] = Empty
> rsequ [x] = Single x
> rsequ xs = Cat (rsequ (allButLast xs)) (Single (last xs))
>
> allButLast :: [a] -> [a]
> allButLast [] = []
> allButLast [_] = []
> allButLast (x:xs) = x: allButLast xs
> test35 :: Assertion
> test35 = assertEqual (Cat (Cat (Single 'a') (Single 'b')) (Single 'c'))
>               (rsequ "abc")

> lsequ :: [a] -> Sequ a
> lsequ [] = Empty
> lsequ [x] = Single x
> lsequ (x:xs) = Cat (Single x) (lsequ xs)

> test34 :: Assertion
> test34 = assertEqual (Cat (Single 'a') (Cat (Single 'b') (Single 'c')))
>               (lsequ "abc")

> sequ :: [a] -> Sequ a
> sequ = lsequ

> normalise:: Sequ a -> Sequ a
> normalise Empty = Empty
> normalise (Single x) = Single x
> normalise (Cat s Empty) = normalise s
> normalise (Cat Empty s) = normalise s
> normalise (Cat a b) = Cat (normalise a) (normalise b) 

> test36 :: Assertion
> test36 = assertEqual 
>     (Cat (Cat (Cat (Cat (Single '2') (Single '3')) (Single '5')) 
>       (Single '7')) (Single '9'))
>   (normalise 
>     (Cat (Cat (Cat (Cat (Single '2') (Single '3')) (Single '5')) 
>       (Single '7')) (Single '9')))
>
> test37 :: Assertion
> test37 = assertEqual 
>   (Cat (Cat (Single '1') (Single '2')) (Single '3'))
>   (normalise (Cat (Cat (Cat (Single '1') Empty) 
>       (Cat (Single '2') Empty)) (Cat Empty (Single '3'))))

> normalised :: (Eq a) => Sequ a -> Bool
> normalised s = s == normalise s

> test38 :: Assertion 
> test38 = assertEqual True
>                      (normalised (Single 'c'))

> -- Attributes are (name:value) pairs.
> data Attribute = Attr String String deriving (Eq)
>
> infixl 4 ===
> (===) :: String -> String -> Attribute
> (===) = Attr

> -- An element is text or tagged
> data XHTMLElement = XHTMLText  String | 
>                     XHTMLElement {tag::String, 
>                                   attributes :: [Attribute],
>                                   content :: Sequ XHTMLElement
>                                  } deriving (Eq)
>
> -- A page is a sequence of elements
> data XHTMLPage = XHTMLPage (Sequ XHTMLElement)
>
> -- Append XHTMLElement to the current Sequ of content elements
> nest :: XHTMLElement -> XHTMLElement -> XHTMLElement
> -- FIXME This points to the need to introduce a super type
> nest (XHTMLText _) _          = error "XHTMLText may not be nested"
> nest XHTMLElement{tag=t, attributes=a, content=c} x = 
>      XHTMLElement{tag=t, attributes=a, content=n} where n = c +++ (Single x)
> infixl 2 <<<
> (<<<) :: XHTMLElement -> XHTMLElement -> XHTMLElement
> (<<<) = nest
>
> -- Add attribute (prepend to attribute list)
> addAttribute :: XHTMLElement -> Attribute -> XHTMLElement
> addAttribute XHTMLElement{tag=t, attributes=as, content=c} a = 
>   XHTMLElement{tag=t, attributes=a:as, content=c}
>   -- FIXME This points to the need to introduce a super type
> addAttribute (XHTMLText _) _ = error "XHTMLText may not have attributes added" 
> infixl 3 @@@ -- bind more closely than nesting
> (@@@) :: XHTMLElement -> Attribute -> XHTMLElement
> (@@@) = addAttribute

\subsection{Outputting XHTML from the representation}

> -- Show name unquoted, value quoted, with spaces before and after
> instance Show Attribute  where
>     show (Attr k v) = " " ++ k ++ "=" ++ show v ++ " "
>     showList []  = showString "" 
>     showList attrs  = showString (concatMap show attrs)

> -- Show an element as text or a possibly empty element 
> instance Show XHTMLElement where 
>  show (XHTMLText s) = s
>  show (XHTMLElement { tag = t, attributes = attrs, content = Empty}) = 
>       "<" ++ t ++ show attrs ++ "/>" 
>  show (XHTMLElement { tag = t, attributes = attrs, content = c}) = 
>    "<" ++ t ++ show attrs ++ ">" ++ (consume show (++) "" c) ++ 
>    "</" ++ t ++ ">\n" 
>
> -- Prepend XML and DTD declarations then show tree
> instance Show XHTMLPage where
>   show (XHTMLPage xs) = xmlDefinition
>                         ++ "\n"
>                         ++ documentTypeDefinition  
>                         ++ "\n"
>                         ++ (consume show (++) "\n" xs)
>                         ++ "\n"
>
>
> -- required at top of document
> xmlDefinition :: String 
> xmlDefinition = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
> -- required immediately after XML definition
> documentTypeDefinition :: String
> documentTypeDefinition = "<!DOCTYPE html PUBLIC " 
>                     ++ ['"'] ++ "-//W3C//DTD XHTML 1.0 Strict//EN"
>                     ++ ['"', ' ', '"']  
>                     ++ "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
>                     ++ ['"', ' ', '>'] 
>
> -- Add enclosing html element
> xhtmlPage :: XHTMLPage -> XHTMLPage
> xhtmlPage (XHTMLPage els) = XHTMLPage (Single XHTMLElement{tag="html",
>                    attributes=["xmlns" === "http://www.w3.org/1999/xhtml"],
>                    content = els})
>
> -- Create an html page from a head and body
> showXHTMLPage :: XHTMLPage -> Sequ Char
> showXHTMLPage xPage = sequ (show $ xhtmlPage xPage)


> -- if the text contains markup characters escape them to entities 
> xhtmlEscape :: String -> XHTMLElement
> xhtmlEscape s = XHTMLText (concatMap tr s) 
>
> tr :: Char -> String
> tr '<' = "&lt;";
> tr '>' = "&gt;"
> tr '&' = "&amp;"
> tr c = [c]
> 
> -- Used when the text contains entities ie is already escaped 
> escaped :: String -> XHTMLElement
> escaped s = XHTMLText s

> test39 :: Assertion
> test39 = assertEqual (escaped "&lt;p&gt;B&amp;O&lt;/p&gt;")
>          (xhtmlEscape  "<p>B&O</p>")

> test40 :: Assertion
> test40 = assertEqual 
>   (Cat (Single 'D') (Cat (Single 'o') (Single 'g')))
>   (sequ "Dog")

> charSequToString :: Sequ Char -> String
> charSequToString Empty = []
> charSequToString (Single c) = [c]
> charSequToString (Cat l r) = (charSequToString l) ++
>                              (charSequToString r) 
> test41 :: Assertion
> test41 = assertEqual
>   "abc"
>   (charSequToString (Cat (Cat (Single 'a')(Single 'b')) (Single 'c')))

\subsubsection{Element creation functions}

> -- Create a simple text element
> element :: String -> String -> XHTMLElement
> element tName s = XHTMLElement 
>                        {tag=tName, 
>                         attributes=[], 
>                         content = (Single $ xhtmlEscape s) }
> 
> -- Create a title 
> title :: String -> XHTMLElement 
> title s = element "title" s 
>
> -- Create an h1
> h1 :: String -> XHTMLElement
> h1 s = element "h1" s 
>
> -- Create a p
> p :: String -> XHTMLElement
> p s = element "p" s
>
> -- Create an img
> img :: String -> String -> String -> String -> 
>       XHTMLElement
> img src alt height width = XHTMLElement {
>   tag="img", 
>   attributes=["src"     === src, 
>               "alt"     === alt,
>               "title"   === alt, -- repeated
>               "height"  === height, 
>               "width"   === width 
>              ], 
>   content = Empty }

> -- Create an anchor with required attribute href
> anchor :: String -> XHTMLElement -> XHTMLElement
> anchor url e = XHTMLElement {
>    tag="a",
>    attributes=["href" === url],
>    content=(Single e)}

\subsubsection{Creating a specific XHMTL instance}

> testText :: String 
> testText = "Hello World <& your dog>"

> xhtmlTestText :: XHTMLElement
> xhtmlTestText = xhtmlEscape testText

> lorem :: XHTMLElement 
> lorem = p (concat ["Lorem ipsum dolor sit amet, consectetur adipiscing ",
>                    "elit. Sed viverra tellus lacus. Curabitur tempus auctor",
>                    "est, pellentesque posuere turpis rhoncus eget. ",
>                    "Pellentesque lacus mi, consectetur et posuere eget, ",
>                    "porta a sem. Aenean vel dictum enim. Pellentesque et ",
>                    "velit ipsum, eu tempor felis. Suspendisse felis tellus, ",
>                    "posuere sit amet ultricies sit amet, luctus id neque. ",
>                    "Donec nulla turpis, tempor a tincidunt nec, eleifend ",
>                    "et turpis. In eget turpis eu nisl consequat gravida sit ",
>                    "amet sit amet urna."])


> helloImg :: XHTMLElement
> helloImg = (img "hello.jpg" (show xhtmlTestText) "78" "298")
>               @@@ "onmouseover" === "style.border='2px solid red';"
> 
>               @@@ "onmouseout"  === "style.border='2px solid green'"
>               @@@ "style"       === "border:2px solid green;"
> 
> imgP :: XHTMLElement
> imgP = XHTMLElement {
>    tag ="p", 
>    attributes=[],
>    content = (Single (anchor "http://google.com" helloImg)) }
>
> br :: XHTMLElement 
> br = XHTMLElement {
>   tag="br", 
>   attributes=[], 
>   content = Empty }
> hr :: XHTMLElement 
> hr = XHTMLElement {
>   tag="hr", 
>   attributes=["style" === "border:2px solid blue;"], 
>   content = Empty }
> validP :: XHTMLElement
> validP = XHTMLElement {
>   tag="p", 
>   attributes=[],
>   content = (Cat (Single br) 
>               (Single (anchor "http://validator.w3.org/check?uri=referer" 
>                        (img "http://www.w3.org/Icons/valid-xhtml10" 
>                             "Valid XHTML 1.0 Strict" "31" "88"))))}
> divBlock :: XHTMLElement 
> divBlock = XHTMLElement{
>   tag="div", 
>   attributes=["style" === "text-align:left; width:640px;"],
>   content=Empty } <<< h1 testText <<< imgP <<< lorem <<< hr

> bodyBlock :: XHTMLElement 
> bodyBlock = XHTMLElement{
>   tag="body", 
>   attributes=["onload" === ("alert('" ++ (show xhtmlTestText) ++ "');"),
>               "style"  === "text-align:center;width:100%"],
>   content= (Single divBlock) } <<< validP

> headBlock :: XHTMLElement
> headBlock = XHTMLElement{
>   tag="head", 
>   attributes=[], 
>   content=(Single $ title testText)}

> testPage :: XHTMLPage
> testPage = XHTMLPage (Cat (Single headBlock) (Single bodyBlock))

> test42 :: Assertion
> test42 = assertEqual 
>   (concat ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
>                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" ",
>                "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" >\n",
>                "<html xmlns=\"http://www.w3.org/1999/xhtml\" >",
>                "<head><title>Hello World &lt;&amp; your dog&gt;</title>\n",
>                "</head>\n",
>                "<body onload=\"alert('Hello World &lt;&amp; your dog&gt;');\"",
>                "  style=\"text-align:center;width:100%\" >",
>                "<div style=\"text-align:left; width:640px;\" >",
>                "<h1>Hello World &lt;&amp; your dog&gt;</h1>\n",
>                "<p><a href=\"http://google.com\" >",
>                "<img ",
>                "style=\"border:2px solid green;\"  ",
>                "onmouseout=\"style.border='2px solid green'\"  ",
>                "onmouseover=\"style.border='2px solid red';\"  ",
>                "src=\"hello.jpg\"  ",
>                "alt=\"Hello World &lt;&amp; your dog&gt;\"  ",
>                "title=\"Hello World &lt;&amp; your dog&gt;\"  ",
>                "height=\"78\"  width=\"298\" ",
>                "/></a>\n",
>                "</p>\n",
>                "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
>                "Sed viverra tellus lacus. Curabitur tempus auctorest, ",
>                "pellentesque posuere turpis rhoncus eget. ",
>                "Pellentesque lacus mi, consectetur et posuere eget, ",
>                "porta a sem. Aenean vel dictum enim. Pellentesque et velit ",
>                "ipsum, eu tempor felis. Suspendisse felis tellus, posuere ",
>                "sit amet ultricies sit amet, luctus id neque. ",
>                "Donec nulla turpis, tempor a tincidunt nec, eleifend et ",
>                "turpis. In eget turpis eu nisl consequat gravida sit amet ",
>                "sit amet urna.</p>\n",
>                "<hr style=\"border:2px solid blue;\" /></div>\n",
>                "<p><br/>",
>                "<a href=\"http://validator.w3.org/check?uri=referer\" >",
>                "<img src=\"http://www.w3.org/Icons/valid-xhtml10\"  ",
>                "alt=\"Valid XHTML 1.0 Strict\"  ",
>                "title=\"Valid XHTML 1.0 Strict\"  ",
>                "height=\"31\"  width=\"88\" /></a>\n",
>                "</p>\n",
>                "</body>\n",
>                "</html>\n\n"
>               ])
>    (charSequToString (showXHTMLPage testPage))
> 

\clearpage
\subsubsection{Outputting an XHMTL file}
If all tests pass then the page is written to the file system.

> output :: IO()
> output = do
>        writeFile "test.xhtml" (charSequToString (showXHTMLPage testPage))

The resulting xhtml file can be seen at \cite{output}
and is shown valid at \cite{validated}. 


\end{document}
