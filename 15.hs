import System.IO
import Data.Char
import Data.String
import Data.List
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime) -- per seed valori casuali


-- Autore: Iacopo Carlini


-- Controllo la validità della mossa e la eseguo se possibile
check_and_play :: [[Int]] -> Int -> Int -> [[Int]]
check_and_play [[]] _ _ = [[]]
check_and_play mat x y
    | x >= (length mat) = mat                                                   -- mossa non valida
    | y >= (length (mat!!1)) = mat                                              -- mossa non valida
    | 0 `elem` (mat !! x) = play_at mat x y                                     -- mossa riga 
    | 0 `elem` ((transpose mat) !! y) = transpose (play_at (transpose mat) y x) -- mossa colonna (faccio due volte la trasposta)
    | otherwise = mat                                   
    
    
-- mossa dati x,y, richiama move per effettuarla
play_at :: [[Int]]-> Int -> Int -> [[Int]]
play_at [[]] x y = [[]]
play_at mat x y = map (move y) mat


-- funzione base mossa su riga (o colonna usando la trasposta)
move :: Int -> [Int] -> [Int]
move y row
    | 0 `elem` row = (take y (filter (/=0) row)) ++ [0] ++ (drop (y) (filter (/=0) row))
    | otherwise = row
    

-- generazione di n valori casuali all'interno di un range
gen_random_numbers_in_range :: Int -> Int -> (Int, Int) -> [Int]
gen_random_numbers_in_range n seed (a, b) = take n $ (randomRs (a, b) myGenerator)
    where
        myGenerator = mkStdGen seed


-- verifica se il primo elemento e il secondo di una tupla sono diversi
asymmetric_tuple :: (Int,Int) -> Bool
asymmetric_tuple t = ((fst t) /= (snd t))


-- controlla se ogni cella della matrice di gioco non corrisponde a quella corretta
-- faccio lo zip fra la matrice disposta su un'unica riga e la soluzione anch'essa su una sola riga
-- se tutte le tuple sono asimmetriche, allora tutte le celle della matrice di gioco sono in posizione sbagliata
all_wrong :: [[Int]] -> Bool
all_wrong [[]] = True
all_wrong mat = and $ map (asymmetric_tuple) (zip (init (concat mat)) (tail $ sort (concat mat)))


-- gioco casuale n volte
play_at_random_n_times :: [[Int]] -> Int -> [Int] -> [[Int]]
play_at_random_n_times mat 1 moves = mat
play_at_random_n_times mat n moves 
    | all_wrong mat == True = mat -- se le celle sono già tutte sbagliate viene interrotta l'esecuzione delle mosse
    | otherwise = play_at_random_n_times (check_and_play mat (head moves) (moves !! 1)) (n-1) (drop 2 moves)


-- controllo condizione di vittoria
-- metto la matrice su un'unica riga usando concat, poi verifico se la matrice privata dell'ultimo elemento,
-- che dovrebbe essere uno zero in condizione di vittoria, coincida con la matrice ordinata e privata del primo elemento
-- perchè nel caso dell'ordinamento lo zero andrebbe a finire in testa 
-- Dunque check_solved ritorna True se e solo se la matrice di gioco è ordinata e ha come ultimo elemento lo zero
check_solved :: [[Int]] -> Bool
check_solved mat = (init (concat mat) == (tail $ sort (concat mat)))



-- loop di gioco ...
playing :: [[Int]] -> IO()
playing mat = 
    do 
        print mat
        
        -- Richiesta salvataggio
        print "Do you want to save the game? y/n"
        answer <- getChar
        
        if (answer == 'y') then do
            writeFile "last_saved.txt" (show mat)
            print "Closing..."
            
        else do
            if (check_solved mat) then do
                print "*** Win ***"
                playing mat -- si continua comunque a giocare...
            
            else do
                print "Insert x: "
                x <- getChar
                print "Insert y: "
                y <- getChar
                let new_matrix = check_and_play mat (digitToInt x) (digitToInt y)
                playing mat
        

-- MAIN
main =
    do
        -- lettura da file
        print "Load from file: "
        filename <- getLine
        input <- readFile filename 
        let matrix = map ( map (read :: String -> Int) ) (map words $ lines input)
        
        let h = length matrix
        let w = length (matrix !! 1)
        print "Your playground: "
        print matrix
        
        
        -- Richiesta applicazione mosse casuali
        
        print "Starting with random moves? y/n"
        answer <- getChar
        
         -- generazione e applicazione mosse casuali 
        if (answer == 'y') then do
            -- uso timestamp (arrotondata al millisecondo) per avere ogni volta generazioni diverse
            seed <- (round . (*1000)) <$> getPOSIXTime 
            let n = 5 -- 5 mosse quindi 5 tuple x,y (genero dunque 5*2 interi)
            let moves = gen_random_numbers_in_range (n*2) seed (0, (max h w)) :: [Int]
            let matrix_after_random_moves = play_at_random_n_times matrix n moves
            
            playing matrix_after_random_moves
        
        -- gioco senza mosse casuali iniziali
        else do
            playing matrix
        
        
