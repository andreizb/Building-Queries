module Query where

import UserInfo
import Rating
import Movie
import Data.Maybe
import Data.List
import Data.Function
import Numeric

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- Functie de split, dupa un caracter
split :: Char -> String -> [String]
split delim [] = [""]
split delim (c:cs)
    | (c == delim && cs /= "") = "" : rest
    | (c == delim && cs == "") = rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split delim cs

-- Citirea unui tabel
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table a b c
            = Table (split a (head (split b c))) (foldr (:) [] (map (split a) (tail (split b c))))

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

-- Construire array de lungimi maxime
max_length_array :: [Entry] -> [Int] -> [Int]
max_length_array [] xs = xs
max_length_array entries xs = max_length_array (tail entries) (f (head entries) xs)
    where
        f [] _ = []
        -- daca gasim un nou maxim, actualizam array-ul pentru pozitia curenta
        f (e:l) (x:xs)
            | (length e) > x = (length e) : (f l xs)
            | otherwise = x : (f l xs)

-- Prelucrare tabel
concat_table :: [Entry] -> [Int] -> [Char]
concat_table [] _ = ""
concat_table (e:entries) xs = (f e xs) ++ "\n" ++ (concat_table entries xs)
    where
        f [] _ = "|"
        f (e:l) (x:xs) = "|" ++ e ++ (replicate (x - (length e)) ' ') ++ (f l xs)

-- Implementare Show
instance Show Table where
    show (Table header entries)
            = y ++ (concat_table [header] x) ++ y ++ (concat_table entries x) ++ y
        where
            x = max_length_array (header:entries) (replicate (length header) 0)
            y = replicate ((foldl (+) 0 x) + ((length header) + 1)) '-' ++ "\n"

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- Implementare getFilter
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt a nr) header = (\y -> read (y !! (fromJust (elemIndex a header))) < nr)
getFilter (Eq a b) header = (\y -> y !! (fromJust (elemIndex a header)) == b)
getFilter (In a b) header
                    = (\y -> ((elemIndex (y !! (fromJust (elemIndex a header))) b) /= Nothing))
getFilter (Not a) header = (\y -> not (getFilter a header y))

data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

-- functie auxiliara
get_entries :: [String] -> TableSchema -> Entry -> Entry
get_entries [] _ _ = []
get_entries (x:xs) header entry
        = (entry !! (fromJust (elemIndex x header))) : (get_entries xs header entry) 

eval :: Query -> Table
eval (Atom x) = x

eval (Select xs q) = f xs (eval q)
    where
        f xs (Table header entries) = Table xs (g xs header entries)
        g _ _ [] = []
        g xs header (e:entries) = (get_entries xs header e) : (g xs header entries)

eval (SelectLimit xs a q) = f xs a (eval q)
    where
        f xs a (Table header entries) = Table xs (g xs a header entries)
        g _ _ _ [] = []
        g xs a header (e:entries)
            | a > 0 = ((get_entries xs header e) : (g xs (a - 1) header entries))
            | otherwise = []

eval (Filter a q) = f a (eval q)
    where
        f a (Table header entries) = Table header (g (getFilter a header) entries)
        g _ [] = []
        g y (e:entries)
            | (y e) == True = (e:(g y entries))
            | otherwise = (g y entries)

eval (a :|| b) = f (eval a) (eval b)
    where
        f (Table h1 e1) (Table h2 e2) = Table h1 (e1 ++ e2)

eval (Cosine q) = Table ["user_id1", "user_id2", "sim"] (start (eval q))
    where
        f (Table header entries)
            | (t entries) /= [] = (t entries) ++ (f (Table header (drop
                                    (length (h entries)) entries)))
            | otherwise = []
        g [] _ = []
        g (x:xs) a
            | (x !! 0) == a = x:(g xs a)
            | otherwise = []
        h [] = []
        h entries = g entries ((head entries) !! 0)
        t entries
            = k (h entries) (drop (length (h entries)) entries) (sum_second (h entries))
        k _ [] _ = []
        k current entries sum_current
            = ((aux ((head current) !! 0) current (h entries) 0 sum_current) :
                (k current (drop (length (h entries)) entries) sum_current))
        aux _ _ [] _ _ = []
        aux idx [] list a b
            | a == 0 = [idx, ((head list) !! 0), "0.0000"]
            | otherwise
                = [idx, ((head list) !! 0), (cos a (sqrt b) (sqrt (sum_second list)))]
        aux idx (e:l1) l2 a b
            | (search_movie (e !! 1) l2) /= []
                = aux idx l1 l2 (sum a (e !! 2) ((search_movie (e !! 1) l2) !! 2)) b
            | otherwise = aux idx l1 l2 a b
        search_movie _ [] = []
        search_movie a (x:xs)
            | a == (x !! 1) = x
            | otherwise = search_movie a xs

        {- Se realizeaza actualizarea numaratorului, daca s-a mai gasit un film evaluat de ambii
           utilizatori, la pasul curent. -}
        sum a b c = a + ((read b) * (read c))
        sum_second [] = 0
        sum_second (x:xs) = ((read (x !! 2)) * (read (x !! 2))) + (sum_second xs)
        
        -- Se realizeaza sortarea lexicografica dupa campul user_id
        start (Table header entries)
            = f (Table header (sortBy (compare `on` (!! 0)) entries))
        cos a b c
            = showFFloat (Just 4) (rounding a b c) ""

-- Functia care calculeaza similaritatea, avand ca parametri numaratorul si radicalii de la numitor
rounding :: Float -> Float -> Float -> Float
rounding a b c = a / (b * c)

-- TODO 5
same_zone :: String -> Query
same_zone x
    = Select ["user_id", "occupation"] (t x)
    where
        f x = (Filter (Eq "user_id" x) (Atom user_info))
        g x = h (eval (Select ["zone"] (f x)))
        h (Table header entries) = concat (entries !! 0)
        t x = Filter (Not (Eq "user_id" x)) (Filter (Eq "zone" (g x)) (Atom user_info))

male_within_age :: Integer -> Integer -> Query
male_within_age a b = Select ["occupation", "zone"] (f a b)
    where
        f a b = Filter (Not (Eq "age" (show a)))
                (Filter (Not (Lt "age" a)) (g b))
        g b = Filter (Lt "age" b) h
        h = Filter (Eq "sex" "M") (Atom user_info)

mixed :: [String] -> [String] -> Int -> Query
mixed a b c = Select ["user_id"] (f a b (toInteger c) (t user_info))
    where
        f [] _ _ a = Atom (Table a [])
        f a (b:xs) c d = (Filter (Eq "occupation" b) (g a c d)) :|| (f xs a c d)
        g [] _ a = Atom (Table a [])
        g (a:xs) b c = (Filter (Eq "zone" a) (h b)) :|| (g xs b c)
        h a = Filter (Lt "age" a) (Atom user_info)
        t (Table header entries) = header
