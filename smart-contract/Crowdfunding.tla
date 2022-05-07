---- MODULE Crowdfunding ----

EXTENDS Integers

CONSTANTS
  BALANCE_LIMIT,
  ADDRESSES,
  MONEY,
  OWNER,
  DEADLINE,
  MAX_BLOCK_NUMBER,
  GOAL,
  SECOND_DEADLINE \* new deadline for getting funds by owner

ASSUMPTION ADDRESSES \subseteq Nat
ASSUMPTION MONEY \subseteq Nat
ASSUMPTION OWNER \in ADDRESSES
ASSUMPTION SECOND_DEADLINE < MAX_BLOCK_NUMBER \* change maximum number of block
ASSUMPTION GOAL <= BALANCE_LIMIT
ASSUMPTION DEADLINE < SECOND_DEADLINE

VARIABLES
  balance, \* contract's balance
  block_number,
  backers,
  key_valid, \* map from address to true/false if sender has a key
  owner_got_funds \* did owner get his money?

vars == <<balance, block_number, backers, key_valid, owner_got_funds>>

TypeOK ==
  /\ balance \in Nat
  /\ block_number \in 0..MAX_BLOCK_NUMBER
  /\ backers \in [ADDRESSES -> 0..BALANCE_LIMIT]
  /\ key_valid \in [ADDRESSES -> BOOLEAN]
  /\ owner_got_funds \in BOOLEAN 

Init ==
  /\ balance = 0
  /\ block_number = 0
  /\ backers = [b \in ADDRESSES |-> 0]
  /\ key_valid = [b \in ADDRESSES |-> TRUE]
  /\ owner_got_funds = FALSE

Tick ==
  /\ block_number + 1 <= MAX_BLOCK_NUMBER
  /\ block_number' = block_number + 1
  /\ UNCHANGED <<balance, backers, key_valid, owner_got_funds>>

SendFunds(sender, amount) ==
  /\ block_number <= DEADLINE

  /\ balance + amount <= BALANCE_LIMIT
  /\ balance' = balance + amount

  /\ backers' = [backers EXCEPT ![sender] = @ + amount]
  /\ key_valid[sender] = TRUE

  /\ UNCHANGED <<block_number, owner_got_funds, key_valid>>

OwnerGetFunds(sender) ==  
  /\ sender = OWNER
  /\ key_valid[sender] = TRUE

  /\ block_number > DEADLINE
  /\ block_number <= SECOND_DEADLINE

  /\ balance >= GOAL
  /\ balance' = 0

  /\ owner_got_funds = FALSE
  /\ owner_got_funds' = TRUE
  
  /\ UNCHANGED <<block_number, backers, key_valid>>

GetReimbursed(sender) ==
  /\ key_valid[sender] = TRUE
  /\ owner_got_funds = FALSE

  /\ (block_number > SECOND_DEADLINE \/ balance < GOAL)
  /\ block_number > DEADLINE

  /\ balance > 0
  /\ balance' = balance - backers[sender]

  /\ backers' = [backers EXCEPT ![sender] = 0]
  /\ UNCHANGED <<block_number, owner_got_funds, key_valid>>

LoseKey(sender) ==
    /\ sender = OWNER
    /\ key_valid[sender] = TRUE
    /\ key_valid' = [key_valid EXCEPT ![sender] = FALSE]
    /\ UNCHANGED <<balance, backers, block_number, owner_got_funds>>

Next ==
  \/ \E <<s, a>> \in ADDRESSES \X MONEY: SendFunds(s, a)
  \/ \E s \in ADDRESSES: OwnerGetFunds(s)
  \/ \E s \in ADDRESSES: GetReimbursed(s)
  \/ \E s \in ADDRESSES: LoseKey(s)
  \/ Tick

Spec == Init /\ [][Next]_vars

\* WF = weak fairness
Fairness ==
  /\ \A s \in ADDRESSES: WF_vars(OwnerGetFunds(s))
  /\ \A s \in ADDRESSES: WF_vars(GetReimbursed(s))
  /\ WF_vars(Tick)

FairSpec == Spec /\ Fairness

DoesNotFreezeFunds ==
  /\ <>(owner_got_funds = TRUE) => <>[](balance = 0)
  /\ [](owner_got_funds = FALSE) => <>[](balance = backers[OWNER])

\* liveness

--------------------------------------

ADDRESSES_const == 0..2
MONEY_const == 0..3

====