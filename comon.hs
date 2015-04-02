
-- from sigfpe
-- http://blog.sigfpe.com/2006/11/variable-substitution-gives.html

class Comonad w where
   counit :: w a -> a
   cobind :: (w a -> b) -> w a -> w b

data Parts' a = A [Parts a] | S String deriving (Eq,Show)
data Parts a = P (Parts' a,a) deriving (Eq,Show)

instance Comonad Parts where
   counit (P (_,a)) = a
   cobind f z = case z of
       P (S s,_) -> P (S s,f z)
       P (A l,_) -> P (A $ map (cobind f) l,f z)


total :: Num a => Parts a -> a
total (P (S _,n)) = n
total (P (A x,_)) = sum $ map total x

lwing, rwing, cockpit, fuselage, body, plane :: Parts Int
lwing = P (S "left wing",100)
rwing = P (S "right wing",100)
cockpit = P (S "cockpit",200)
fuselage = P (S "fuselage",200)
body = P (A [fuselage,cockpit],undefined)
plane = P (A [lwing,rwing,body],undefined)

test4 :: Parts Int
test4 = cobind total plane

