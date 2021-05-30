// reach
'reach 0.1';

// vote function
function contest (){
    // three voter variables - user input boolean
    // vote 1 for contestant 1 and 0 for contestant 0
    var Voter0, Voter1, Voter2
    Voter0 = input boolean
    Voter1 = input boolean
    Voter2 = input boolean
    // two contestant variables - user input boolean
    var contestant0, contestant1
    contestant0 = 0
    contestant1 = 0
    
    // add votes
    total_votes = Voter0 + Voter1 + Voter2

    // divide votes by 3
    consensus = total_votes / 3
    if consensus > 0.5:
        return 'contestant1 wins'
    else:
        return 'contestant0 wins'
}

contest()