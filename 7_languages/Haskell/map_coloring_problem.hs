-- different = [
--      ("M", "T"),("M", "A"),
--      ("A", "T"),("A", "M"),("A", "G"),("A", "F"),
--      ("G", "F"),("G", "T")]

-- mapping of state to color
colors = ["red", "green", "blue"]
colorings = [[("Alabama", c1), ("Mississippi", c2), ("Georgia", c3), ("Tennessee", c4), ("Florida", c5)] |
    c1 <- colors, c2 <- colors, c3 <- colors, c4 <- colors, c5 <- colors,
    c2 /= c4, c2 /= c1,
    c1 /= c4, c1 /= c3, c1 /= c4,
    c3 /= c5, c3 /= c4]