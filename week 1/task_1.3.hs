twoString s_1 s_2
    | s_1 == "hello" && s_2 == "world" = "Hello!"
    | s_1 == "hello" = "Hello, but I am not " ++ s_2
    | otherwise = "Sorry, I do not understand"