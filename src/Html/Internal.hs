{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Html.Internal where

-- types
newtype HTML = HTML {getHTML :: String} deriving (Eq, Semigroup, Monoid)

instance Show HTML where
  show (HTML str) = str


newtype Structure = Structure {getStruct :: String}
  deriving (Eq, Semigroup, Monoid)

instance Show Structure where
  show (Structure str) = str


type Title = String


-- basic helper functions


wrapStartTag :: String -> String
wrapStartTag tagName = '<': tagName ++ ">"

wrapEndTag :: String -> String
wrapEndTag tagName = wrapStartTag ('/': tagName)


wrapTags_' :: String -> String -> String
wrapTags_' tagName content =
    wrapStartTag tagName <> content <> wrapEndTag tagName

-- NOTE: the commented implementation allows for indentation
-- wrapTags_' :: String -> Bool -> String -> String
-- wrapTags_' tagName indent content =
--     wrapStartTag tagName <> content <> indentStr <> wrapEndTag tagName
--     where indentStr = if indent
--                       then "\n"
--                       else ""
      

wrapTags_ :: String -> String -> Structure
wrapTags_ tagName content = Structure $ wrapTags_' tagName content


-- wrapping in tag functions

html_ :: Title -> Structure -> HTML
html_ title (Structure contents)= HTML $
  wrapTags_' "html" 
    (
       wrapTags_' "head" (wrapTags_' "title" (escapeHTML title))
    <> wrapTags_' "body" contents
    )

p_ :: String -> Structure
p_ = wrapTags_ "p" . escapeHTML

h1_ :: String -> Structure
h1_ = wrapTags_ "h1" . escapeHTML

-- NOTE: the commented implementations of lists have indentation 
-- l_ :: String -> [Structure] -> Structure
-- l_ typeOfList xs = Structure $ wrapTags_' typeOfList True finalStruct
--   where f (Structure struct) = Structure $ "\n  " <> wrapTags_' "li" False struct
--         (Structure finalStruct) = foldMap f xs

-- ul_ :: [Structure] -> Structure
-- ul_ = l_ "ul"

-- ol_ :: [Structure] -> Structure
-- ol_ = l_ "ol"

l_ :: String -> [Structure] -> Structure
l_ listType = wrapTags_ listType . concatMap (wrapTags_' "li" . getStruct)

ul_ :: [Structure] -> Structure
ul_ = l_ "ul"

ol_ :: [Structure] -> Structure
ol_ = l_ "ol"

code_ :: String -> Structure
code_ = wrapTags_ "pre" . escapeHTML



-- other

escapeHTML :: String -> String
escapeHTML = let escapeChar c = case c of
                                  '<' -> "&lt;"
                                  '>' -> "&gt;"
                                  '&' -> "&amp;"
                                  '"' -> "&quot;"
                                  '\'' -> "&#39;"
                                  _ -> [c]
              in concatMap escapeChar

