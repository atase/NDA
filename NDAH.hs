
{-# LANGUAGE LambdaCase #-}
{-

{p, ¬p} |- r

1. {p, ¬p}|- p; ( Ipoteza ̆) 
2. {p, ¬p}|- ¬p; ( Ipoteza ̆)
3. {p, ¬p}|- ⊥; (¬e, 1, 2)
4. {p, ¬p}|- r. ( ⊥e, 3)
-}

letters = ['p','q','r','s','t','u','v','x','y','z']
connectors = ['~', '&', '|', '>', '<','-']
symbols = ['(', ')', ',', '{', '}']

formula = ""
goal = ""
premises = ""
results = []

main :: IO ()
main = do
  putStrLn "\n[******** NATURAL DEDUCTION ASSISTENT *********]"
  putStrLn "type help if you need help\n"
  interface (0, [],[])


interface :: (Int, [String], [Tree]) -> IO ()
interface (count, solve, trees) = do
	command <- promptLine "> "
	if length (command) == 0 then
		executeDefault(count, solve, trees)
	else
		case head (words command) of{
					"exit" 						-> executeExit (command, count, solve, trees);
					"help" 						-> executeHelp (command, count, solve, trees);
					"load"						-> executeLoad (command, count, solve, trees);
					"refresh"					-> interface (0,[],[]);
					"hypothesis"				-> executeHypothesis (command, count, solve, trees);
					
					"and_introduction" 			-> executeAndIntroduction (command, count, solve, trees);
					"and_elimination_first"		-> executeAndEliminationFirst (command, count, solve, trees);
					"and_elimination_second"	-> executeAndEliminationSecond (command, count, solve, trees);

					"or_introduction_first"		-> executeOrIntroductionFirst (command, count, solve, trees);
					"or_introduction_second"	-> executeOrIntroductionSecond (command, count, solve, trees);
					"not_not_elimination"		-> executeNotNotElimination (command, count, solve, trees);
					"not_elimination"			-> executeNotElimination (command, count, solve, trees);
					"null_elimination"			-> executeNullElimination (command, count, solve, trees);
					"not_introduction"			-> executeNotIntroduction (command, count, solve, trees);
				    "or_elimination"			-> executeOrElimination (command, count, solve, trees);
					"implies_elimination"		-> executeImpliesElimination (command, count, solve, trees); 
					"extension"					-> executeExtension (command, count, solve, trees);
					"implies_introduction"		-> executeImpliesIntroduction (command, count, solve, trees); 

					_							-> executeDefault (count, solve, trees); 
					}

{-
	[***** EXECUTE COMMANDS *****]
-}

executeExit :: (String, Int, [String], [Tree]) -> IO ()
executeExit (command,count, solve, trees) = do {
					if length (words command) > 1
						then do 
							putStrLn $ invalidCommand;
							interface (count, solve, trees);
					else do
						putStrLn ("\n[******* EXIT ! HAVE A NICE DAY !  *******]\n");
						return();
				};

executeHelp :: (String, Int, [String], [Tree]) -> IO ()
executeHelp (command,count, solve, trees) = do {
							if length (words command) > 1
								then do 
									putStrLn $ invalidCommand;
									interface (count, solve, trees);
							else do
									putStrLn ("\n[******* HELP  *******]\n");
									putStrLn $ displayHelp;
									interface (count, solve, trees);
						};


executeLoad :: (String, Int, [String], [Tree]) -> IO()
executeLoad (command,count, solve, trees) = do {
							if length (words command) == 2
								then do
									if validAlphabet (head (tail (words command))) == True
										then do
											let (count', solve', trees', errors) = loadFormula (head (tail (words command)), count, solve, trees);
											if errors == ""
												then do
													putStrLn $ displayLines (reverseList solve');
													interface (count', solve', trees');
											else do
												putStrLn $ errors;
												interface (count', solve', trees');
									else do 
										putStrLn $ invalidCommand;
										interface (count, solve, trees);
							else do 
								putStrLn $ invalidCommand;
								interface (count, solve, trees);
						};


executeHypothesis :: (String, Int, [String], [Tree]) -> IO ()
executeHypothesis (command,count, solve, trees) = do{
								if length (words command) == 3
									then do 
										let (count', solve', trees', errors) = hypothesis (tail (words command), count, solve, trees)
										if errors == ""
											then do
												putStrLn $ displayLines (reverseList solve');
												interface (count', solve', trees');
										else do
											putStrLn $ errors;
											interface (count', solve', trees');
								else do
									putStrLn $ invalidCommand;
									interface (count, solve, trees);

							}; 


executeAndIntroduction :: (String, Int, [String], [Tree]) -> IO ()
executeAndIntroduction (command, count, solve, trees) = do {
									if length (words command) == 3
										then do 
											let (count', solve', trees', errors) = andIntroduction (tail (words command), count, solve, trees);
										--	
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
										else do
											putStrLn $ invalidCommand;
											interface (count, solve, trees);
									};

executeAndEliminationFirst :: (String, Int, [String], [Tree]) -> IO ()
executeAndEliminationFirst (command, count, solve, trees) = do {
										if length (words command) == 2
											then do 
												let (count', solve', trees', errors) = andEliminationFirst (tail (words command), count, solve, trees);
												if errors == ""
													then do 
														putStrLn $ displayLines (reverseList solve');
														if head(trees') == last(trees')
															then do
																putStrLn "\n[ ***** SOLVED *****]\n";
																interface (count', solve', trees');
														else do
															interface (count', solve', trees');
													else do
														putStrLn $ errors;
														interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
											};

executeAndEliminationSecond :: (String, Int, [String], [Tree]) -> IO ()
executeAndEliminationSecond (command, count, solve, trees) = do {
											if length (words command) == 2
												then do 
													let (count', solve', trees', errors) = andEliminationSecond (tail (words command), count, solve, trees);
													if errors == ""
													then do 
														putStrLn $ displayLines (reverseList solve');
														if head(trees') == last(trees')
															then do
																putStrLn "\n[ ***** SOLVED *****]\n";
																interface (count', solve', trees');
														else do
															interface (count', solve', trees');
													else do
														putStrLn $ errors;
														interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
										};


executeOrIntroductionFirst :: (String, Int, [String], [Tree]) -> IO ()
executeOrIntroductionFirst (command, count, solve, trees) = do {
										if length (words command) == 3
											then do
												let (count', solve', trees', errors) = orIntroductionFirst (tail (words command), count, solve, trees);
												if errors == ""
													then do 
														putStrLn $ displayLines (reverseList solve');
														if head(trees') == last(trees')
															then do
																putStrLn "\n[ ***** SOLVED *****]\n";
																interface (count', solve', trees');
														else do
															interface (count', solve', trees');
													else do
														putStrLn $ errors;
														interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
										};


executeOrIntroductionSecond :: (String, Int, [String], [Tree]) -> IO ()
executeOrIntroductionSecond (command, count, solve, trees) = do {
										if length (words command) == 3
											then do
												let (count', solve', trees', errors) = orIntroductionSecond (tail (words command), count, solve, trees);
												if errors == ""
													then do 
														putStrLn $ displayLines (reverseList solve');
														if head(trees') == last(trees')
															then do
																putStrLn "\n[ ***** SOLVED *****]\n";
																interface (count', solve', trees');
														else do
															interface (count', solve', trees');
													else do
														putStrLn $ errors;
														interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
										};

executeNotNotElimination :: (String, Int, [String], [Tree]) -> IO ()
executeNotNotElimination (command, count, solve, trees) = do {
									if length (words command) == 2
										then do 
											let (count', solve', trees', errors) = notnotElimination (tail (words command), count, solve, trees);
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
										else do
											putStrLn $ invalidCommand;
											interface (count, solve, trees);
									};
executeNotElimination :: (String, Int, [String], [Tree]) -> IO ()
executeNotElimination (command, count, solve, trees) = do {
									if length (words command) == 3
										then do
											let (count', solve', trees', errors) = notElimination (tail (words command), count, solve, trees);
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
								};

executeNullElimination :: (String, Int, [String], [Tree]) -> IO ()
executeNullElimination (command, count, solve, trees) = do {
									if length (words command) == 3
										then do
											let (count', solve', trees', errors) = nullElimination (tail (words command), count, solve, trees);
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
										else do
											putStrLn $ invalidCommand;
											interface (count, solve, trees);
								};
executeNotIntroduction :: (String, Int, [String], [Tree]) -> IO ()
executeNotIntroduction (command, count, solve, trees)  = do {
									if length (words command) == 3
										then do
											let (count', solve', trees', errors) = notIntroduction (tail (words command), count, solve, trees);
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
										else do
											putStrLn $ invalidCommand;
											interface (count, solve, trees);
								};

executeOrElimination :: (String, Int, [String], [Tree]) -> IO ()
executeOrElimination (command, count, solve, trees) = do {
								if length (words command) == 4
									then do
										let (count', solve', trees', errors) = orElimination (tail (words command), count, solve, trees);
										putStrLn $ displayLines (reverseList solve');
										if errors == ""
											then do 
												putStrLn $ displayLines (reverseList solve');
												if head(trees') == last(trees')
													then do
														putStrLn "\n[ ***** SOLVED *****]\n";
														interface (count', solve', trees');
												else do
													interface (count', solve', trees');
											else do
												putStrLn $ errors;
												interface (count', solve', trees');
									else do
										putStrLn $ invalidCommand;
										interface (count, solve, trees);
								}; 

executeImpliesElimination :: (String, Int, [String], [Tree]) -> IO ()
executeImpliesElimination (command, count, solve, trees) = do {
									if length (words command) == 3
										then do
											let (count', solve', trees', errors) = impliesElimination (tail (words command), count, solve, trees);
											if errors == ""
												then do 
													putStrLn $ displayLines (reverseList solve');
													if head(trees') == last(trees')
														then do
															putStrLn "\n[ ***** SOLVED *****]\n";
															interface (count', solve', trees');
													else do
														interface (count', solve', trees');
												else do
													putStrLn $ errors;
													interface (count', solve', trees');
										else do
											putStrLn $ invalidCommand;
											interface (count, solve, trees);
									}; 

executeExtension :: (String, Int, [String], [Tree]) -> IO ()
executeExtension (command, count, solve, trees) = do{
							if length (words command) == 3
								then do
									let (count', solve', trees', errors) = extension (tail (words command), count, solve, trees);
									if errors == ""
										then do 
											putStrLn $ displayLines (reverseList solve');
											if head(trees') == last(trees')
												then do
													putStrLn "\n[ ***** SOLVED *****]\n";
													interface (count', solve', trees');
												else do
													interface (count', solve', trees');
											else do
												putStrLn $ errors;
												interface (count', solve', trees');
									else do
										putStrLn $ invalidCommand;
										interface (count, solve, trees);
							};

executeImpliesIntroduction :: (String, Int, [String], [Tree]) -> IO ()
executeImpliesIntroduction (command, count, solve, trees) = do { 
										if length (words command) == 3
											then do
												let (count', solve', trees', errors) = impliesIntroduction (tail (words command), count, solve, trees);
												if errors == ""
													then do 
														putStrLn $ displayLines (reverseList solve');
														if head(trees') == last(trees')
															then do
																putStrLn "\n[ ***** SOLVED *****]\n";
																interface (count', solve', trees');
															else do
																interface (count', solve', trees');
														else do
															putStrLn $ errors;
															interface (count', solve', trees');
											else do
												putStrLn $ invalidCommand;
												interface (count, solve, trees);
									}; 

executeDefault :: (Int, [String], [Tree]) -> IO ()
executeDefault	(count, solve, trees) = do {
						putStrLn $ invalidCommand;
						interface (count, solve, trees);
					};


{-
	[*** Functions ***]

-}

promptLine :: String -> IO String
promptLine prompt = do
					putStr prompt
					getLine			

reverseList :: [String] -> [String]
reverseList [] = []
reverseList xs = last xs : reverseList (init xs)

reverseTreesList :: [Tree] -> [Tree]
reverseTreesList [] = []
reverseTreesList xs = last xs : reverseTreesList (init xs)

displayLines :: [String] -> String
displayLines [] = []
displayLines (c:lines) = c ++ displayLines lines 

invalidCommand :: String
invalidCommand = "[!!!] Invalid command ! Press help to see the correct syntax."

displayHelp :: String
displayHelp = "*** exit - no argument, close shell\n \
			 \ \n*** help - no argument, display commands with short description\n \
			 \ \n*** load [sentence] - 1 argument (ex. load {~p&q}|-~p), load formula for proof\n \
			 \ \n*** hypothesis [sentence] - 1 argument (ex. hypothesis p&q - IN FORMULA), set hypothesis p&q to use \n \
			 \ \n*** and_introduction [line, line] - 2 arguments (ex. and_introduction 1 2) <=> {F|-p, F|-q} => F|-p&q\n \
			 \ \n*** and_elimination_first [line] - 1 argument (ex. and_elimination_first 1) <=> {F|-p&q} => F|-p\n \
			 \ \n*** and_elimination_second [line] - 1 argument (ex. and_elimination_second 1) <=> {F|-p&q} => F|-q\n \
			 \ \n*** or_introduction_first [line, sentence] - 2 arguments (ex. or_introduction_first 1 p&q) <=> {F|-p,p&q} => F|-p||(p&q)\n \
			 \ \n*** or_introduction_second [line, sentence] - 2 arguments (ex. or_introduction_second 1 p&q) <=> {p&q, F|-p} => F|-(p&q)||p\n \
			 \ \n*** or_elimination [line, line, line] - 3 arguments (ex. or_elimination 1 2 3) <=> {F|-p||q, {F,p}|-r, {F,q}|-r} => F|-r \n \
			 \ \n*** implies_introduction [line, sentence] - 2 arguments (ex. implies_introduction 1 p&q) <=> {{F,p&q}|-r} => F|- {p&q->r}\n \
			 \ \n*** implies_elimination [line, line] - 2 arguments (ex. implies_elimination 1 2) <=> {F|-{p->q}, F|-p} => F|-q\n \
			 \ \n*** not_introduction [line, sentence] - 2 arguments (ex. not_introduction 1 p) <=> {{F,p}|-null} => F|-(~p)\n \
			 \ \n*** not_elimination [line, line] - 2 arguments (ex. not_elimination 1 2) <=> {F|-p,F|-(~p)} => F|-null\n \
			 \ \n*** null_elimination [line, sentence] - 2 arguments (ex. null_elimination 1 p&q) <=> {F|-null} => F|-p&q\n \
			 \ \n*** not_not_elimination [line] - 1 argument (ex. not_not_elimination 1) <=> {F|-(~~p)} => F|-p\n \
			 \ \n*** extension [line, sentence] - 2 arguments (ex. extension 1 p&q) <=> {F|-p} => {F,p&q}|-p\n"


{-
	[***** BASIC VALIDATE DATA *****]
-}

validAlphabet :: String -> Bool
validAlphabet "" = True
validAlphabet (x:xs)
	| x `elem` letters 		= True && validAlphabet xs
	| x `elem` connectors 	= True && validAlphabet xs
	| x `elem` symbols 		= True && validAlphabet xs
	| otherwise 			= False 


validConsecutive :: [Char] -> Bool
validConsecutive [x] = True
validConsecutive (x:xs)
			| x `elem` letters && head (xs) `elem` letters 	= False
			| otherwise 									= True && validConsecutive xs


{-
	[***** COMMANDS *****]
-}

impliesIntroduction :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
impliesIntroduction (args, count, solve, trees)
	| count == 0                           = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS < 0 				           = (count, solve, trees, "Argument should be >= 0") 
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| sentenceTree `notElem` premises      = (count, solve, trees, "Second argument should be in premises list, check the list! ")
	| length premises == 0				   = (count, solve, trees, "You can't apply this command on a empty list of premises ! ")
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")  
	where
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));

		tree = trees' !! (lineS);
			
		premises = getPremises tree;
		newTokens = tokenize(newPremises (sentenceTree, premises));
		newTree = buildTree newTokens;

		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;		
		
		resultTree = ProofNode newTree (ImpliesNode sentenceTree rightSubtree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(->, E, " ++ show lineS ++ ", " ++ showTree sentenceTree ++ ")\n";

newPremises :: (Tree,[Tree]) -> String
newPremises (_, []) = ""
newPremises (tree, x:xs)
	| tree == x = newPremises (tree, xs)
	| otherwise = 
		case nextChar of
			""  -> (showTree x) 
			_   -> (showTree x) ++ "," ++ (newPremises (tree, xs))
		where nextChar = newPremises (tree, xs)
{-
	******************* EXTENSION 
-}

extension :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
extension (args, count, solve, trees)
	| count == 0                           = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS < 0 				           = (count, solve, trees, "Argument should be >= 0") 
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| sentenceTree `elem` premises         = (count, solve, trees, "You already have that premise, check the list ! ")
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")  
	where
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));
		tree = trees' !! (lineS);

		premises = getPremises tree;

		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;

		newPremises = (CommaNode leftSubtree sentenceTree)

		resultTree = ProofNode  newPremises sentenceTree;
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(hyp/extension, " ++ show lineS ++ ", " ++ showTree sentenceTree ++ ")\n";

impliesElimination :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
impliesElimination (args, count, solve, trees)
	| count < 2 = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !") 
	| (isInt (args !! 0) == False)   = (count, solve, trees, "First argument should be a number !")
	| (isInt (args !! 1) == False)   = (count, solve, trees, "Second argument should be a number !") 
	| line1 >= count 				    = (count, solve, trees, "First argument should be < " ++ show count)
	| line2 >= count 				    = (count, solve, trees, "Second argument should be < " ++ show count) 
	| line1 < 0 				        = (count, solve, trees, "First argument should be >= 1")
	| line2 < 0 				        = (count, solve, trees, "First argument should be >= 1")
	| line1 == line2 					= (count, solve, trees, "Lines should be different ! ")
	| leftSubtree1 /= leftSubtree2	    = (count, solve, trees, "Be sure to select two lines which have same premises !")
	| (validImplies rightSubtree1 == False) = (count, solve, trees, "The first line should have the form {P}|-(f1->f2) !")
	| rightSubtree2 /= leftSubtree11 = (count, solve, trees, "Incorrect! Line 1 should have the form {P}|-(f1->f2) and line 2 should have the form {P}|-f1.")
	| resultTree `elem` (tail trees')   = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                         = (count+1, line:solve, resultTree:trees,"")
	where
	    trees' = reverseTreesList trees;
		line1 = read (args !! 0) :: Int;
		line2 = read (args !! 1) :: Int;

		tree1 = trees' !! (line1);
		tree2 = trees' !! (line2);

		leftSubtree1 = getLeftSubtree tree1;
		rightSubtree1 = getRightSubtree tree1;

		leftSubtree11 = getLeftSubtree rightSubtree1;
		rightSubtree12 = getRightSubtree rightSubtree1;

		leftSubtree2 = getLeftSubtree tree2;
		rightSubtree2 = getRightSubtree tree2;

		{-
			1. leftSubtree1 == leftSubtree2
			2. rightSubtree2 == leftSubtree11
		-}

		resultTree = ProofNode leftSubtree1 rightSubtree12;
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(->, E, " ++ show line1 ++ "," ++ show line2 ++ ")\n";

validImplies :: Tree -> Bool
validImplies (ImpliesNode _ _) = True
validImplies _ = False


orElimination :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
orElimination (args, count, solve, trees)
	| count < 3 = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !") 
	| (isInt (args !! 0) == False)   = (count, solve, trees, "First argument should be a number !")
	| (isInt (args !! 1) == False)   = (count, solve, trees, "Second argument should be a number !")
	| (isInt (args !! 2) == False)   = (count, solve, trees, "Third argument should be a number !")  
	| line1 >= count 				    = (count, solve, trees, "First argument should be < " ++ show count)
	| line2 >= count 				    = (count, solve, trees, "Second argument should be < " ++ show count)
	| line3 >= count 				    = (count, solve, trees, "Third argument should be < " ++ show count)  
	| line1 < 0 				        = (count, solve, trees, "First argument should be >= 0")
	| line2 < 0 				        = (count, solve, trees, "Second argument should be >= 0")
	| line3 < 0 				        = (count, solve, trees, "Third argument should be >= 0")
	| line1 == line2 					= (count, solve, trees, "Line 1 and line 2 should be different ! ")
	| line2 == line3 					= (count, solve, trees, "Line 2 and line 3 should be different ! ")
	| line1 == line3 					= (count, solve, trees, "Line 1 and line 3 should be different ! ")
	| rightSubtree2 /= rightSubtree3    = (count, solve, trees, "Line from second argument should have identical conclusion with line from third argument")
	| (validOr rightSubtree1 == False)  = (count, solve, trees, "The first line should have the form {P}|-(f1||f2) !")
	| ((orElems !! 0) `notElem` premisesTree2) = (count, solve, trees, showTree (orElems !! 0) ++ " should be premise in tree from argument 2 !")
	| ((orElems !! 1) `notElem` premisesTree3) = (count, solve, trees, showTree (orElems !! 1) ++ " should be premise in tree from argument 3 !")
	| newPrem1 /= newPrem2              = (count, solve, trees, "Criterias for this command is not true on line 1 or line 2, check the list or type help !")
	| newPrem2 /= newPrem3              = (count, solve, trees, "Criterias for this command is not true on line 2 or line 3, check the list or type help !")
	| newPrem1 /= newPrem3              = (count, solve, trees, "Criterias for this command is not true on line 1 or line 3, check the list or type help !")
	| resultTree `elem` (tail trees')   = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                         = (count+1, line:solve, resultTree:trees,"") 
	where trees' = reverseTreesList trees;
		line1 = read (args !! 0) :: Int;
		line2 = read (args !! 1) :: Int;
		line3 = read (args !! 2) :: Int;

		tree1 = trees' !! (line1);
		tree2 = trees' !! (line2);
		tree3 = trees' !! (line3);

		leftSubtree2 = getLeftSubtree tree2;
		rightSubtree2 = getRightSubtree tree2;

		leftSubtree3 = getLeftSubtree tree3;
		rightSubtree3 = getRightSubtree tree3;

		premisesTree1 = getPremises tree1;
		premisesTree2 = getPremises tree2;
		premisesTree3 = getPremises tree3;

		leftSubtree1 = getLeftSubtree tree1;
		rightSubtree1 = getRightSubtree tree1;

		orElems = getOrElems rightSubtree1;

		newPrem1 = newPremises (Empty, premisesTree1);
		newPrem2 = newPremises ((orElems !! 0), premisesTree2);
		newPrem3 = newPremises ((orElems !! 1), premisesTree3);


		resultTree = ProofNode leftSubtree1 rightSubtree2;
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(||, E, " ++ show line1 ++ "," ++ show line2 ++ "," ++ show line3 ++ ")\n";

validOr :: Tree -> Bool
validOr (OrNode _ _ ) = True
validOr _ = False

getOrElems :: Tree -> [Tree]
getOrElems (OrNode left right) = [left, right]

{-

	****************** NOT INTRODUCTION
-}
notIntroduction :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
notIntroduction (args, count, solve, trees)
	| count < 3                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "Argument should be >= 1") 
	| (validNull rightSubtree == False)    = (count, solve, trees, "The line doesn't contains a tree of the form {P}|-_ , try again !") 
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| sentenceTree == Empty                = (count, solve, trees, "Second argument should be a valid formula, try to use '(' and ')' if you have 2 connectors consecutive!")
	| sentenceTree `notElem` premises      = (count, solve, trees, "The second argument should be in premises, check line 0 !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"") 
	where 
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));

		tree = trees' !! (lineS);
		premises = getPremises tree;
		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;
		resultTree = ProofNode leftSubtree (NotNode sentenceTree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(~, I, " ++ show lineS ++ ")\n";

{-
	******************** NULL ELIMINATION

-}

nullElimination :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
nullElimination (args, count, solve, trees)
	| count < 3                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "Argument should be >= 1") 
	| (validNull rightSubtree == False)    = (count, solve, trees, "The line doesn't contains a tree of the form {P}|-_ , try again !") 
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"") 
	where 
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));
		tree = trees' !! (lineS);
		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;
		resultTree = ProofNode leftSubtree sentenceTree;
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(_, E, " ++ show lineS ++ ")\n";

validNull :: Tree -> Bool
validNull (TBoxNode c) = True
validNull _ = False

{-

	************** NOT ELIMINATION
-}

notElimination :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
notElimination (linesNo, count, solve, trees)
	| count < 3 = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !") 
	| (isInt (linesNo !! 0) == False)   = (count, solve, trees, "First argument should be a number !")
	| (isInt (linesNo !! 1) == False)   = (count, solve, trees, "Second argument should be a number !") 
	| line1 >= count 				    = (count, solve, trees, "First argument should be < " ++ show count)
	| line2 >= count 				    = (count, solve, trees, "Second argument should be < " ++ show count) 
	| line1 <= 0 				        = (count, solve, trees, "First argument should be >= 1")
	| line2 <= 0 				        = (count, solve, trees, "First argument should be >= 1")
	| leftSubtree1 /= leftSubtree2	    = (count, solve, trees, "Be sure to select two lines which have same premises !")
	| (validNot rightSubtree2 == False) = (count, solve, trees, "The second line should have the form {P}|-(~f) !")
	| rightSubtree1 /= (getNotFormula rightSubtree2) = (count, solve, trees, "Incorrect! Line 1 should have the form {P}|-f and line 2 should have the form {P}|-(~f).")
	| resultTree `elem` (tail trees')   = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                         = (count+1, line:solve, resultTree:trees,"")
	where
		trees' = reverseTreesList trees;
		line1 = read (linesNo !! 0) :: Int;
		line2 = read (linesNo !! 1) :: Int;
		tree1 = trees' !! (line1);
		tree2 = trees' !! (line2);
		leftSubtree1 = getLeftSubtree tree1;
		rightSubtree1 = getRightSubtree tree1;
		leftSubtree2 = getLeftSubtree tree2;
		rightSubtree2 = getRightSubtree tree2;
		resultTree = ProofNode leftSubtree1 (TBoxNode '_');
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(~, E, " ++ show line1 ++ "," ++ show line2 ++ ")\n";

validNot :: Tree -> Bool
validNot (NotNode tree) = True
validNot _ = False

getNotFormula :: Tree -> Tree
getNotFormula (NotNode tree) = tree
getNotFormula _ = Empty

{-

	************* NOT NOT ELIMINATION
-}

notnotElimination :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
notnotElimination (lineNo, count, solve, trees)
	| count < 2                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (lineNo !! 0) == False)       = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "Argument should be >= 1") 
	| (validNotNot rightSubtree == False)  = (count, solve, trees, "The line doesn't contain a tree of the form ~~f1, try again !") 
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")
	where 
		trees' = reverseTreesList trees;
		lineS = read (lineNo !! 0) :: Int;
		tree = trees' !! (lineS);
		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;
		resultTree = ProofNode leftSubtree (getNotNotFormula(rightSubtree));
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(~~, E, " ++ show lineS ++ ")\n";

validNotNot :: Tree -> Bool
validNotNot (NotNotNode tree) = True
validNotNot _ = False


{-


******************* OR INTRODUCTION


-}
orIntroductionSecond :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
orIntroductionSecond (args, count, solve, trees)
	| count < 2                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "First argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "First argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "First argument should be >= 1")
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")
	where trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));
		tree = trees' !! (lineS);
		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;
		resultTree = ProofNode leftSubtree (OrNode sentenceTree rightSubtree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(||, I2, " ++ show lineS ++ "," ++ showTree sentenceTree ++ ")\n";



orIntroductionFirst :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
orIntroductionFirst (args, count, solve, trees)
	| count < 2                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (args !! 0) == False)         = (count, solve, trees, "First argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "First argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "First argument should be >= 1")
	| (validAlphabet (args !! 1) == False) = (count, solve, trees, "Invalid alphabet, type help if you have problems. ")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula sentenceTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode sentenceTree /= 0     = (count, solve, trees, "You should not have proof symbol ! Please, try again. :)") 
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")
	where
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		sentenceTree = buildTree (tokenize (args !! 1));
		tree = trees' !! (lineS);
		leftSubtree = getLeftSubtree tree;
		rightSubtree = getRightSubtree tree;
		resultTree = ProofNode leftSubtree (OrNode rightSubtree sentenceTree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(||, I1, " ++ show lineS ++ ", " ++ showTree sentenceTree ++ ")\n";



{-

	****************** AND ELIMINATION
-}

andEliminationSecond :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
andEliminationSecond (lineNo, count, solve, trees)
	| count < 2                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (lineNo !! 0) == False)       = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "Argument should be >= 1")
	| (validAndTree rightSubtree == False) = (count, solve, trees, "The line doesn't contain a tree of the form f1 && f2, try again !")
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")
	where trees' = reverseTreesList trees;
		lineS = read (lineNo !! 0) :: Int;
		tree = trees' !! (lineS);
		rightSubtree = getRightSubtree tree;
		leftSubtree = getLeftSubtree tree;
		resultTree = ProofNode leftSubtree (getRightSubtree rightSubtree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(&&, E2, " ++ show lineS ++ ")\n";

andEliminationFirst :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
andEliminationFirst (lineNo, count, solve, trees)
	| count < 2                            = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !")
	| (isInt (lineNo !! 0) == False)       = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				       = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS <= 0 				           = (count, solve, trees, "Argument should be >= 1")
	| (validAndTree rightSubtree == False) = (count, solve, trees, "The line doesn't contain a tree of the form f1 && f2, try again !")
	| resultTree `elem` (tail trees')      = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                            = (count+1, line:solve, resultTree:trees,"")
	where 
		trees' = reverseTreesList trees;
		lineS = read (lineNo !! 0) :: Int;
		tree = trees' !! (lineS);
		rightSubtree = getRightSubtree tree;
		leftSubtree = getLeftSubtree tree;
		resultTree = ProofNode leftSubtree (getLeftSubtree rightSubtree);
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(&&, E1, " ++ show lineS ++ ")\n";

validAndTree :: Tree -> Bool
validAndTree (AndNode left right) = True
validAndTree tree = False



{-
	AND INTRODUCTION

-}
andIntroduction :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
andIntroduction (linesNo, count, solve, trees)
	| count < 3 = (count, solve, trees, "Not enough waypoints to use command ! You should set the goal and hypothesis !") 
	| (isInt (linesNo !! 0) == False) = (count, solve, trees, "First argument should be a number !")
	| (isInt (linesNo !! 1) == False) = (count, solve, trees, "Second argument should be a number !") 
	| line1 >= count 				  = (count, solve, trees, "First argument should be < " ++ show count)
	| line2 >= count 				  = (count, solve, trees, "Second argument should be < " ++ show count) 
	| line1 <= 0 				      = (count, solve, trees, "First argument should be >= 1")
	| line2 <= 0 				      = (count, solve, trees, "First argument should be >= 1")
	| leftSubtree1 /= leftSubtree2	  = (count, solve, trees, "Be sure to select two lines which have same premises !")
	| resultTree `elem` (tail trees') = (count, solve, trees, "You already have this formula, check the list !")
	| otherwise                       = (count+1, line:solve, resultTree:trees,"")
	where 
		trees' = reverseTreesList trees;
		line1 = read (linesNo !! 0) :: Int;
		line2 = read (linesNo !! 1) :: Int;
		tree1 = trees' !! (line1);
		tree2 = trees' !! (line2);
		leftSubtree1 = getLeftSubtree tree1;
		leftSubtree2 = getLeftSubtree tree2;
		rightSubtree1 = getRightSubtree tree1;
		rightSubtree2 = getRightSubtree tree2;
		resultTree = ProofNode leftSubtree1 (AndNode rightSubtree1 rightSubtree2)
		line = " " ++ show count ++ ". " ++  showTree resultTree ++ " ........(&&, I, " ++ show line1 ++ ", " ++ show line2 ++ ")\n"; 

loadFormula :: (String, Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
loadFormula (sentence, count, solve, trees)
	| count > 0                 = (count, solve, trees, "You already have a goal ! Type refresh if you want to load a new formula !")  
	| (validAlphabet sentence == False)	 = (count, solve, trees, "Invalid alphabet! Type help if you have a problem")
	| (atLeastOneLetter sentence == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen sentence /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets sentence /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (validTree tree' == False)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| countProofNode tree' /= 1 = (count, solve, trees, "You should have one proof symbol ! Please, try again. :)")
	| otherwise                 = (count+1, line:solve, tree':trees,"") 
	where 
		tree' = buildTree (tokenize sentence);
		line = " " ++ show count ++ ". Proof that: " ++ showTree tree' ++ "\n"

hypothesis :: ([String], Int, [String], [Tree]) -> (Int, [String], [Tree], [Char])
hypothesis (args, count, solve, trees)
	| count == 0                         = (count, solve, trees, "You should set the goal first !")
	| (isInt (args !! 0) == False)       = (count, solve, trees, "Argument should be a number !")
	| lineS >= count 				     = (count, solve, trees, "Argument should be < " ++ show count)
	| lineS < 0 				         = (count, solve, trees, "Argument should be > 0")
	| (validAlphabet (args !! 1) == False)	 = (count, solve, trees, "Invalid alphabet! Type help if you have a problem")
	| (atLeastOneLetter (args !! 1) == False) = (count, solve, trees, "Formula should have at least one letter (p,q,t,u,v,w,x,y,z).")
	| (equalNumberOfParen (args !! 1) /= 0) = (count, solve, trees, "Formula should have ')' as many as '('")
	| (equalNumberOfBrackets (args !! 1) /= 0) = (count, solve, trees, "Formula should have '{' as many as '}'")
	| (isValidFormula userTree == 0)		 = (count, solve, trees, "There is a problem with formula, try to use brackets if you have two consecutive connectors !")
	| resultTree `elem` (tail trees')    = (count, solve, trees, "You already have this hypothesis, check the list !")
	| userTree `notElem` premises        = (count, solve, trees, "Formula should be in premises list , check the list!")
	| otherwise                          = (count+1, line:solve, resultTree:trees,"") 
	where
		trees' = reverseTreesList trees;
		lineS = read (args !! 0) :: Int;
		tree = trees' !! (lineS);
		premises = getPremises tree;
		leftSubtree = getLeftSubtree tree;

		userTree = buildTree (tokenize (args !! 1));
		resultTree = ProofNode leftSubtree userTree;
		line = " " ++ show count ++ ". " ++ showTree resultTree ++ " ........(hyp)\n";


{- VALIDATE USER ARGUMENT-}
isInt :: String -> Bool 
isInt str = 
	case (reads str) :: [(Int, String)] of
		[(_,"")]  -> True
		_         -> False

atLeastOneLetter :: String -> Bool
atLeastOneLetter [] = False
atLeastOneLetter (x:xs)
	| x `elem` letters = True
	| otherwise = atLeastOneLetter xs

equalNumberOfBrackets :: String -> Int
equalNumberOfBrackets [] = 0
equalNumberOfBrackets (x:xs)
	| x == '{' = 1 + equalNumberOfBrackets xs
	| x == '}' = equalNumberOfBrackets xs - 1
	| otherwise = equalNumberOfBrackets xs 

equalNumberOfParen :: String -> Int
equalNumberOfParen [] = 0
equalNumberOfParen (x:xs)
	| x == '(' = 1 + equalNumberOfParen xs
	| x == ')' = equalNumberOfParen xs - 1
	| otherwise = equalNumberOfParen xs

isValidFormula :: Tree -> Int
isValidFormula Empty = 0
isValidFormula (LetterNode x) = 1
isValidFormula (TBoxNode c) = 1
isValidFormula (AndNode left right) = isValidFormula left * isValidFormula right
isValidFormula (OrNode left right) = isValidFormula left * isValidFormula right
isValidFormula (ImpliesNode left right) = isValidFormula left * isValidFormula right
isValidFormula (NotNode tree) = 1 * isValidFormula tree
isValidFormula (NotNotNode tree) = 1 * isValidFormula tree
isValidFormula tree = 1

validTree :: Tree -> Bool
validTree Empty = False
validTree (ProofNode left right)
	| isValidFormula left == 0  = False
	| isValidFormula right == 0 = False
	| otherwise = True
validTree _ = False

{-
	[***** TOKENS *****]

-}
isLetter :: Char -> Bool
isLetter l = elem l "pqrstuvwxyz"

isConnector :: Char -> Bool
isConnector c = elem c "~,&|->"

isSpace :: Char -> Bool
isSpace c = c == ' '

data Connector = And | Or | Not | Proof | Implies | NotNot | Comma | InvalidConnector
			deriving (Show, Eq)

data Token =  TLetter Char 
			| TConnector Connector
			| TLeftParen
			| TRightParen
			| TLeftBracket
			| TRightBracket
			| TBox Char
			| TEnd
			deriving (Show, Eq)


connector :: [Char] -> Connector
connector c | c == "&&"  = And
			| c == "||"  = Or
			| c == "~~"  = NotNot
			| c == "~"   = Not
			| c == "|-"  = Proof
			| c == ","   = Comma
			| c == "->"  = Implies
			| otherwise  = InvalidConnector

connectorSymbol :: String -> (String,String)
connectorSymbol str = symbols "" str
	where
		symbols :: String -> String -> (String, String)
		symbols acc [] = (acc, [])
		symbols acc (c:cs)  | isConnector c = 
								let(acc', cs') = symbols acc cs
								in (c:acc', cs')
							| otherwise = (acc, c:cs)

connect c cs = 
	let (str, cs') = connectorSymbol cs
	in  TConnector (connector (c:str)) : tokenize cs'


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:rest)
	| isConnector c = connect c rest
	| isLetter c    = TLetter c : tokenize rest
    | isSpace c 	= tokenize rest
    | c == '_'		= TBox c: tokenize rest
    | c == '('		= TLeftParen : tokenize rest
    | c == ')'		= TRightParen : tokenize rest
    | c == '{'		= TLeftBracket : tokenize rest
    | c == '}'		= TRightBracket : tokenize rest
	| otherwise = error $ "Cannot tokenize " ++ [c]


{-
	[***** PARSER + TREE *****]

-}


data Tree =  Empty
			| AndNode Tree Tree
			| OrNode Tree Tree
			| ProofNode Tree Tree
			| ImpliesNode Tree Tree
			| CommaNode Tree Tree
			| NotNode Tree
			| NotNotNode Tree
			| LetterNode Char
			| TBoxNode Char
		deriving (Show,Eq)


lookAhead :: [Token] -> Token
lookAhead [] = TEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

sentence :: [Token] -> (Tree, [Token])
sentence toks = 
	let (formulaTree, toks') = letter toks
    in
		case lookAhead toks' of
			(TConnector conn) | elem conn [And] ->
				let (senTree, toks'') = sentence (accept toks')
				in (AndNode formulaTree senTree, toks'')

			(TConnector conn) | elem conn [Or] ->
				let (senTree, toks'') = sentence (accept toks')
				in (OrNode formulaTree senTree, toks'')

			(TConnector conn) | elem conn [Proof] ->
				let (senTree, toks'') = sentence (accept toks')
				in (ProofNode formulaTree senTree, toks'')

			(TConnector conn) | elem conn [Implies] ->
				let (senTree, toks'') = sentence (accept toks')
				in (ImpliesNode formulaTree senTree, toks'')

			(TConnector conn) | elem conn [Comma] ->
				let (senTree, toks'') = sentence (accept toks')
				in (CommaNode formulaTree senTree, toks'')

			_ -> (formulaTree, toks')


letter :: [Token] -> (Tree, [Token])
letter toks = 
	case lookAhead toks of
		(TLetter l)       -> (LetterNode l, accept toks)

		(TBox b)		  -> (TBoxNode b, accept toks)

		(TConnector conn) | elem conn [Not] ->
			let (expTree, toks') = letter (accept toks)
			in (NotNode expTree, toks')

		(TConnector conn) | elem conn [NotNot] ->
			let (expTree, toks') = letter (accept toks)
			in (NotNotNode expTree, toks')

		(TLeftParen) ->
			let (expTree, toks') = sentence (accept toks)
			in
				if lookAhead toks' /= TRightParen
				then (Empty, [])
				else (expTree, accept toks')

		(TLeftBracket) ->
			let (expTree, toks') = sentence (accept toks)
			in
				if lookAhead toks' /= TRightBracket
				then (Empty, [])
				else (expTree, accept toks')

		_ -> (Empty, [])


buildTree :: [Token] -> Tree
buildTree [] = Empty;
buildTree toks = let (tree, toks') = sentence toks
             in
               if null toks' 
               then tree
               else Empty

checkGoal :: (Tree, Tree) -> Bool
checkGoal (x,y)
	| x == y = True
	| otherwise = False

showTree :: Tree -> String
showTree (LetterNode x) =  [x] 
showTree (NotNode tree)   = " (~" ++ showTree tree ++ ")"
showTree (NotNotNode tree) = "(~~" ++ showTree tree ++ ")"
showTree (AndNode left right) = "(" ++ showTree left ++ " && " ++ showTree right ++ ")"
showTree (OrNode left right) = "(" ++ showTree left ++ " || " ++ showTree right ++ ")"
showTree (ProofNode left right) = "{" ++ showTree left ++ "}" ++ " |- " ++ showTree right
showTree (ImpliesNode left right) = "(" ++ showTree left ++ " -> " ++ showTree right ++ ")"
showTree (CommaNode left right) = showTree left ++ ", " ++ showTree right
showTree (TBoxNode b) = [b]
showTree (Empty) = ""

getPremises :: Tree -> [Tree]
getPremises (ProofNode left right) = getPremises left
getPremises (CommaNode left right) = getPremises left ++ getPremises right
getPremises (LetterNode x) = [LetterNode x]
getPremises tree = [tree] 

getLeftSubtree :: Tree -> Tree
getLeftSubtree (LetterNode x) = LetterNode x
getLeftSubtree (ProofNode left right) = left
getLeftSubtree (ImpliesNode left right) = left
getLeftSubtree (AndNode left right) = left

getRightSubtree :: Tree -> Tree
getRightSubtree (LetterNode x) = LetterNode x
getRightSubtree (ProofNode left right) = right
getRightSubtree (ImpliesNode left right) = right
getRightSubtree (AndNode left right) = right

getNotNotFormula :: Tree -> Tree
getNotNotFormula (NotNotNode tree) = tree;
getNotNotFormula _ = Empty

countProofNode :: Tree -> Int
countProofNode (LetterNode x) = 0
countProofNode (ProofNode left right) = 1 + countProofNode left + countProofNode right
countProofNode _ = 0