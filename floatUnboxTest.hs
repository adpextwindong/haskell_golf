foo :: Float -> Float -> Float
foo x y = 3 * (x - (-y))

main = return (foo 3 4)
