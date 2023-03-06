# Tennis Stats

## Probability contributors:
- Surface (within a range, 10yrs ago != today)
- Elo
- *??? Dominant hand ???*
- Previous results with the current opponent.
- Results of last X matches (mental state, ++ loses = ++bad mood | -- loses = --bad mood)
  - >> Maybe after Y loses motivation to win kicks in or opponent gets cocky
  - >> Maybe after Z wins, player gets cocky and looses
  - Get avarage streak of wins & loses.


## To Do:
- Do we calculate the probability of winning set by set, thus getting the match result, or directly the match outcome?
- What is a statistial hypothesis?
- How to test hypothesis?
- Create a hypothesis on our project
- Test it


## Useful links:
https://www.ultimatetennisstatistics.com

https://github.com/JeffSackmann/tennis_atp


- - -


## Data

### Data columns:
- `tourney_id`: Tournament ID 
- `tourney_name`: Tournament NAME
- `surface`: On which surface players played.
- `draw_size`: Initial first round of pairing of entrants
- `tourney_level`: 
- `tourney_date`: date
- `match_num`: 
- `winner_id`: the id of the player that won
- `winner_seed`: strongest players in tournaments (1=TOP)
- `winner_entry`: how the played entered the tournaments
- `winner_name`: players name
- `winner_hand`: hand with which the player played
- `winner_ht`: height
- `winner_ioc`: nationality
- `winner_age`: age
- `loser_id`: "
- `loser_seed`: "
- `loser_entry`: "
- `loser_name`: "
- `loser_hand`: "
- `loser_ht`: "
- `loser_ioc`: "
- `loser_age`: "
- `score`: resulting score
- `best_of`: match duration/number of sets
- `round`: 	(F) finalist; (SF) semifinalist; (QF) quarterfinalist; (#R) rounds 4, 3, 2, 1; (RR) round-robin stage; (Q#) qualification round; (P#) preliminary round; 
- `minutes`: minutes played
- `w_ace`: NUmber of aces? (A legal serve which the returner does not manage to get their racquet to)
- `w_df`: number of double faults (Two serving faults in a row in one point, causing the player serving to lose the point.)
- `w_svpt`: total serve points
- `w_1stIn`:  Number of First serves in
- `w_1stWon`: Number of points won on 1st serve
- `w_2ndWon`: Number of points won on 2nd serve
- `w_SvGms`: 
- `w_bpSaved`: 
- `w_bpFaced`: 
- `l_ace`: 
- `l_df`:
- `l_svpt`: 
- `l_1stIn`: 
- `l_1stWon`: 
- `l_2ndWon`: 
- `l_SvGms`: 
- `l_bpSaved`: 
- `l_bpFaced`: 
- `winner_rank`:
- `winner_rank_points`:
- `loser_rank`:
- `loser_rank_points`:


### Data that we have before the match:
- `tourney_id`: Tournament ID 
- `tourney_name`: Tournament NAME
- `surface`: On which surface players played.
- `draw_size`: Initial first round of pairing of entrants
- `tourney_level`: 
- `tourney_date`: date
- `match_num`: 
- PLAYER 1 DATA
- PLAYER 2 DATA
- Weather (?)