**Members: Anson Tran (aht55), Eugene Choi (ec727), David Han (dmh338), Andy He (ah734)**

**Meeting Plan:** We will meet on Wednesdays, and Fridays, and Sundays, after dinner (approximately 8 PM). If necessary, we will meet on Saturdays as well.

**System proposal:**

An extension of popular New York Times game Wordle that includes a generator, solver, and user interface.

Key features:

- A user interface to play Wordle, just as how the game is usually presented: users will have 6 tries to guess a certain word. For each correct letter in the correct position, it will be indicated with green. For a correct letter in the wrong spot, it will be indicated with yellow.
- A Wordle AI solver that will optimally solve wordle in at most 3 to 4 guesses (for 5 letter words).
- The option to play against the Wordle AI solver
- Extensions to classic wordle: option to change word length, enforce timer
- A terminal-based visualizer that provides insight into how our algorithm solves a Wordle puzzle
- Database for user and AI statistics, compiling information such as games played, streaks, guess distribution. This will also be used to test the accuracy of our algorithm.

**Roadmap:**

**MS1 (Alpha):**

Satisfactory Scope: First, we must set up the project git repository and invite all members of the group as collaborators. We begin our project by setting up a framework to promote the paradigm of test driven development (TDD). This begins with the creation of .mli files that outline the modules and methods we intend to implement, just as how it is presented to us in A2. For MS1, this involves breaking down our project into 4 components: the game, the solver, an engine to test our solver, and the visualizer. Outlining our main methods prior to implementation provides several benefits. First, it allows us to develop the test suite. Second, it will allow members working on different components of the project to work in parallel and promote orthogonality and modularity in our code. Lastly, by providing extensive documentation on our planned methods, we hold ourselves accountable for planned functionality.

Good Scope: We plan to use tracer bullet development (TBD) to provide us real-time feedback on the development of later modules. This also has a two-fold impact, as it allows us to release demos throughout each phase of our development. To accomplish this, we first design and implement a playable command line wordle game. The user (or some other user) will input a random 5-letter word for the player to guess in 6 tries. Just like in the normal Wordle game, the player will receive feedback on their guesses (indicated by green, yellow, or gray block letters). Not only will this allow us to better understand the core functionality of the game, but it will also provide us with a framework to test and debug our solving algorithm as it enters development for MS2.

Excellent Scope: For excellent scope, we will begin the development for our solver algorithm. The first step in this process would be to access the set of valid wordle guesses and words and save them locally using plaintext. We will then preprocess the word lists by analyzing key features such as letter frequency, positioning of letters, and other statistics. These too, will be saved as plaintext. We also begin adding to our knowledge portfolio by researching existing approaches, learning about information theory, and brainstorming improvements to current algorithms.

**MS2 (Beta):** What is our estimate for MS2? &quot;We&#39;ll get back to you&quot;

Just kidding.😊

Satisfactory Scope: First, we develop a framework to test the performance of our wordle solver. To do this, we will automate the process of testing against all past and future words and compiling statistics regarding an algorithm&#39;s performance, such as average guesses per puzzle.

Good Scope: Develop a naive algorithm that makes guesses based on simple word statistics such as relative letter frequency and letter placement. We hope that this algorithm will at least solve 90% of Wordles using no more than 6 guesses (the maximum allowed by the website).

Excellent Scope: Refine our algorithm further by implementing a guessing algorithm based on information theory. We would like to choose words that maximize the expected entropy per guess. Further improvements could be made using lookahead approaches. We hope this algorithm will solve every wordle in 3-4 guesses on average.

**MS3 (Release)**

Satisfactory Scope: A GUI that offers a more aesthetically pleasing presentation of a playable Wordle game and a demonstration of an AI Wordle Solver. Competition against the AI should be intuitive: following each guess, the display of both games (that of the user and the AI) will be updated, showing the correct letters guessed by each, while concealing the literal guesses of the AI.

Good Scope: Extensions to Wordle, such as different word lengths and timers, should be implemented in the GUI as well, extending the functionality of our game (not to mention the AI) beyond the classic New York Times Wordle.

Excellent Scope: The GUI should offer easy access to the database storing user and AI statistics, as well as an optional analysis of games (retrospective reviews comparing progression of single-player games to more optimal decisions).