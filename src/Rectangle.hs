module Rectangle where



-- Cartesian coordinates
type Coord = (Int, Int)

-- A rectangle
type Rect = (Coord, Coord)


--split a recangle into 4 pieces if the coord is inside the rectangle, one otherwise
split :: Rect -> Coord -> [Rect]
split r@(r1, r2) c
  | c>r1 && c >r2 = [r]
  | c<r1 && c<r2 = [r]
  | otherwise  = [(r1, c), (a, d), (b, r2)]
  where
    a = (fst c, snd r1)
    b = (fst r1, snd c)
    d = (fst c, snd r2)


-- Add 2 rectangles getting remaining r1, remaining r2 and the intersection
add :: Rect -> Rect -> ([Rect], [Rect], Rect)
add r1@(r11, r12) r2@(r21, r22) = undefined
  where
    

