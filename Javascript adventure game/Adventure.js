// Function to display output
function displayOutput(output) {
  console.log(output); // For Node.js, writes to the terminal
  // Or use document.write(output); // For browser, writes to the HTML document
}

// Function to get user input
function getUserInput(question) {
  return prompt(question); // For browser, uses prompt for user input
}

// The story
function startGame() {
  displayOutput("You find yourself standing at the entrance of a mysterious cave. Legends say it is home to a powerful artifact, but also many dangers.");
  let choice = getUserInput("Do you want to enter the cave? (yes/no)");

  if (choice.toLowerCase() === "yes") {
      displayOutput("You light a torch and step into the darkness, the entrance quickly disappearing behind you.");
      exploreCave();
  } else {
      displayOutput("You decide not to enter the cave. Perhaps another day.");
  }
}

function exploreCave() {
  displayOutput("As you venture deeper, the air grows colder and the walls close in. You hear strange whispers echoing around you.");
  let choice = getUserInput("Do you want to continue forward? (yes/no)");

  if (choice.toLowerCase() === "yes") {
      displayOutput("You move cautiously, trying to ignore the eerie feeling creeping up your spine.");
      forkInThePath();
  } else {
      displayOutput("You decide to turn back. The entrance seems impossibly far away now.");
  }
}

function forkInThePath() {
  displayOutput("You arrive at a fork in the path. One path is dimly lit with a faint, warm glow. The other is shrouded in darkness.");
  let choice = getUserInput("Which path do you choose? (lit/dark)");

  if (choice.toLowerCase() === "lit") {
      displayOutput("You follow the light, which leads you to a chamber with a beautiful, glowing crystal.");
      endGame("Congratulations, you have found the Crystal of Light! You win!");
  } else if (choice.toLowerCase() === "dark") {
      displayOutput("You bravely venture into the darkness, but the path twists and turns, and soon you are hopelessly lost.");
      endGame("You wander the cave endlessly, never finding your way out. Game over.");
  } else {
      displayOutput("You hesitate, unsure of which path to take.");
      forkInThePath(); // Restart the function to prompt for a valid choice
  }
}

function endGame(message) {
  displayOutput(message);
  let playAgain = getUserInput("Do you want to play again? (yes/no)");

  if (playAgain.toLowerCase() === "yes") {
      startGame(); // Restart the game
  } else {
      displayOutput("Thank you for playing!");
  }
}

// Start the game
startGame();
