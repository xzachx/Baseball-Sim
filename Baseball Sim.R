

events <- c('1b','2b','3b','hr','bb','k','o');

inning <- c(1,0);
outs <- 0;
score <- c(0,0);
runners <- c(0,0,0);

getAtbat <- function() {
  events[sample(1:7,1)];
}

gameOver <- function() {
  if (all(inning == c(9,1)) && score[1] < score[2]) {
    TRUE;
  }
  else if (all(inning == c(10,0)) && score[1] != score[2]) {
    TRUE;
  }
  else if (inning[1] > 9 && inning[2] == 0 && score[1] != score[2]) {
    TRUE;
  }
  else {
    FALSE;
  }
}

# Game loop
while (!gameOver()) {

  # Inning loop
  while(outs < 3) {
    
    # Determine outcome of current at bat
    atbat <- getAtbat();

    # Single
    if (atbat == '1b') {
      if (runners[3] == 1) {
        score[inning[2] + 1] <- score[inning[2] + 1] + 1;
        runners[3] <- 0;
      }
      if (runners[2] == 1) {
        runners[2:3] <- c(0,1);
      }
      if (runners[1] == 1) {
        runners[2] <- 1;
      }
      runners[1] <- 1;
    }
    
    # Double
    else if (atbat == '2b') {
      score[inning[2] + 1] <- score[inning[2] + 1] + sum(runners[2:3]);
      if (runners[1]) {
        runners <- c(0,1,1);
      }
      else {
        runners <- c(0,1,0);
      }
    }
    
    # Triple
    else if (atbat == '3b') {
      score[inning[2] + 1] <- score[inning[2] + 1] + sum(runners);
      runners <- c(0,0,1);
    }
    
    # Homerun
    else if (atbat == 'hr') {
      score[inning[2] + 1] <- score[inning[2] + 1] + sum(runners) + 1;
      runners <- c(0,0,0);
    }
    
    # Walk
    else if (atbat == 'bb') {
      if (sum(runners) == 3) {
        score[inning[2] + 1] <- score[inning[2] + 1] + 1;
      }
      else if (!runners[1]) {
        runners[1] <- 1;
      }
      else if (!runners[2]) {
        runners[1:2] <- c(1,1);
      }
      else {
        runners <- c(1,1,1);
      }
    }
    
    # Strikeout or batted ball out
    else if (atbat == 'k' || atbat == 'o') {
      outs <- outs + 1;
    }
  }
  
  # Advance inning
  outs <- 0
  if (inning[2]) {
#    inning[1] <- inning[1] + 1;
#    inning[2] <- 0;
    inning <- c(inning[1] + 1, 0);
  }
  else {
    inning[2] <- 1;
  }
  
}
inning;
score;
