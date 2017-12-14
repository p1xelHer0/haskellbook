module ReviewOfCurrying where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1.
-- appedCatty "woohoo!" > "woops mrow woohoo!"
-- 2.
-- frappe "1"
--   flip cattyConny "haha" "1"
--     cattyConny "1" "haha"
--       "1 mrow haha"
-- 3.
-- frappe (appedCatty "2")
--   frappe (cattyConny "woops" "2")
--     frappe "woops mrow 2"
--       "woops mrow 2 mrow haha"
--
-- 4.
-- appedCatty (frappe "blue")
--   appedCatty ("blue mrow haha")
--     "woops mrow blue mrow haha"
--
-- 5.
-- cattyConny (frappe "pink")
--            (cattyConny "green" (appedCatty "blue"))
--   cattyConny (frappe "pink")
--              (cattyConny "green" ("woops mrow blue"))
--      cattyConny (frappe "pink")
--                 ("green mrow woops mrow blue"))
--        cattyConny ("pink mrow haha")
--                   ("green mrow woops mrow blue"))
--          cattyConny "pink mrow haha" "green mrow woops mrow blue"
--            "pink mrow haha mrow green mrow woops mrow blue"
--
-- 6.
-- cattyConny (flippy "Pugs" "are") "awesome"
--   cattyConny ("are mrow Pugs") "aresome"
--     "are mrow Pugs mrow awesome"
