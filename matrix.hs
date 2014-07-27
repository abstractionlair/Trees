import qualified Data.Vector as V

element i j = (i,j)

makerow cols row = V.generate cols (element row)

make cols rows = V.generate rows (makerow cols)


-- [ [ f 0 0, f 0 1, f 0 2 ],
--   [ f 1 0, f 1 1        ],
--   [ f 2 0               ] ]
makeULTD f n = let makerow row = V.generate (n-row) (f row)
               in V.generate n makerow

-- [ [ f 0 0, f 0 1, f 0 2 ],
--   [        f 1 1, f 1 2 ],
--   [               f 2 2 ] ]
makeURTD f n = let g i j = f i (j+i)
                   makerow row = V.generate (n-row) (g row) 
               in V.generate n makerow

-- [ [ f 1 2, f 1 3, f 1 4 ],
--   [        f 2 3, f 2 4 ],
--   [               f 4 4 ] ]
makeURT f n = let g i j = f (i+1) (j+i+2)
                  makerow i = V.generate (n-i) (g i)
              in V.generate n makerow 

-- [ [ f 0 0,              ]
--   [ f 1 0, f 1 1,       ]
--   [ f 2 0, f 2 1, f 2 2 ] ]
makeLLTD f n = let makerow row = V.generate (1+row) (f row)
               in V.generate n makerow

-- [ [ f 1 0,              ]
--   [ f 2 0, f 2 1,       ]
--   [ f 3 0, f 3 1, f 3 2 ] ]
makeLLT f n = let g i j = f (i+1) j 
                  makerow row = V.generate (1+row) (g row)
              in V.generate n makerow
