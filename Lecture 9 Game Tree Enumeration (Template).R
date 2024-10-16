rm(list = ls())

#===============================================================================
# Populate the state matrix
#===============================================================================
S = matrix(0,9,8)
# Row Sums:
S[1:3, 1] = 1
S[4:6, 2] = 1
S[7:9, 3] = 1
# Col Sums:
S[c(1:3)*3-2,4] = 1
S[c(1:3)*3-1,5] = 1
S[c(1:3)*3-0,6] = 1
# Diag Sums
S[c(1,5,9),7] = 1
S[c(3,5,7),8] = 1

S
#===============================================================================
# Evaluate the Game State rho(m,S)
#===============================================================================
rho = function(m,S)
{
  m = matrix(m,ncol = 1)
  player1 = any(t(m)%*%(S)==-3) # X
  player2 = any(t(m)%*%(S)==3) # O
  m_1     = (m==-1) #m_
  m_2     = (m==1) #m+
  tie     = sum((t(m_1)%*%S >0) * (t(m_2)%*%S >0)) == 8
  winner  = c(-1,0,1)[c(player1, tie, player2)]
  terminal = player1 | tie | player2
  ret     = list(terminal = terminal, winner = winner)
  return(ret)
}

m = as.matrix(c(1,1,-1,0,0,0,0,0,-1)) # game
matrix(m,3,3,byrow = TRUE)
rho(m,S)

m = as.matrix(c(1,1,-1,0,0,-1,0,0,-1)) 
matrix(m,3,3,byrow = TRUE)
rho(m,S)

#===============================================================================
# game_tree(m,k) returns a vector of -1, 0, and +1s delineating all terminal 
# game states.
#===============================================================================
game_tree= function(m,k)
{
  g = c()
  game_state = rho(m,S)
  if(game_state$terminal){
    g = c(g, game_state$winner)
  }else {
    Index = which(m == 0)
    for(i in 1:length(Index)){
      x = m
      x[Index[i]] = k
      g = c(g, game_tree(x, -1*k))
    }
  }
  return(g)
	
}
m = as.matrix(c(0,0,0,0,0,0,0,0,0)) # open Board
#m = as.matrix(c(1,1,-1,0,0,0,0,0,-1))



res = game_tree(m,-1)
n_g  = 	length(res)
Xwins = sum(res==-1)
Draws = sum(res== 0)
Owins = sum(res==+1)
c(n_g,Xwins,Draws,Owins)

#===============================================================================
# Write a function that draws the game tree.
#===============================================================================
draw_game_tree

