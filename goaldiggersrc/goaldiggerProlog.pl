/**
 * Prolog Knowledge Base of Agent
 *
 */

% Bool Switches
% true enables debug features logging, timing etc
:- dynamic lDebugOn/0.
% true enables scoring board
:- dynamic scoringBoardOn/0.
% switch to deactivate double speed for workers
:- dynamic activateDoubleSpeed/0.
% Switch that signals Main Module to get active
:- dynamic haveMove/0.
% switch to recognize simulations have different characteristics on sim change
:- dynamic expectDifferentSimulations/0.
% switch timing step calculation of agent
:- dynamic activateTimer/0.
% prevents reinit in first sim
:- dynamic thisIsTheFirstSim/0.
% switch to send calculated offsets to everyone
:- dynamic calculatedNewOffset/0.
% switch to recalculate dispenses distance
:- dynamic calculateNewDispenserMD/0.
% switch to recalculate goalzone distance
:- dynamic calculateNewGoalzoneMD/0.
% switch to drop all attached blocks
:- dynamic dropAllBlocks/0.
% switch for misc debug prints
:- dynamic printsAllowed/0.
% switch for MAS crash handling
:- dynamic simulationCrashed/1.

% waiting for norms ending
%:- dynamic waitingForNorms/0.

% switch for norm handling
:- dynamic activateNormHandling/0.

% when a marked is north of agent.
:- dynamic northExplored/0.
% when a marked is south of agent.
:- dynamic southExplored/0.
% when a marked is east of agent.
:- dynamic eastExplored/0.
% when a marked is west of agent.
:- dynamic westExplored/0.

% switch to stop logging score after x sims
:- dynamic stopScoreLogging/1.
% switch to stop logging duration after x sims
:- dynamic stopDurationLogging/1.
% to calculate duration for a sim with unix timestamp
:- dynamic simDuration/1.
% switch for saboteur
:- dynamic activateSaboteurFeature/0.

% when a marked is north of agent.
:- dynamic currentCustomRoleStatus/1.

% current score
:- dynamic currentScore/1.
% Counter for sim in tournament mode
:- dynamic simCount/1.
% agents count his failed 1tasks
:- dynamic countFailedTask/2.
% agents count his submitted 1tasks
:- dynamic count1Task/2.
% agents count his submitted 2tasks
:- dynamic count2Task/2.
% agents count his submitted 3tasks
:- dynamic count3Task/2.
% agents count his submitted 3tasks
:- dynamic count4Task/2.
% cached so it survives map change for score table
:- dynamic cachedCountFailedTask/1.
:- dynamic cachedCount1Task/1.
:- dynamic cachedCount2Task/1.
:- dynamic cachedCount3Task/1.
:- dynamic cachedCount4Task/1.

% (Bool, Dir)
:- dynamic haveBlockAttached/2.
% switch dispenser delivered block (Bool, Step)
:- dynamic haveDispenserDelivery/2.


% step counter in belief of simulation
:- dynamic step/1.
% timing step duration for agent
:- dynamic elapseStepTime/1.
% Coordinates of agent XY
:- dynamic agentAt/3.
% nswe direction preferred for exploration
:- dynamic randomAffinity/1.
% nswe direction in witch should be explored
:- dynamic exploreDirection/1.
% Variables for Manhatten Distance
:- dynamic targetMd/3, nMd/1, sMd/1, wMd/1, eMd/1, executeManhattan/0.
% (X,Y,BlockType,MD) of dispenser to search out for
:- dynamic targetDispenserAt/4.
% closest of all known dispensers (X,Y,Blocktype, MD)
:- dynamic targetClosestOfAllDispensersAt/4.
% do skip/explore until this step
:- dynamic skipThisStep/1.
% as told changes random affinity
:- dynamic changeAffinityAfterTheseSteps/1.
% task the agent has chosen and works on (TaskName, TaskStep, Reward, X, Y, BlockType, Client/Server, NameSubmitter)
:- dynamic currentChosenTask/8.
% dispenser percept data plus MD (X,Y,Type,Details,MD)
:- dynamic mapDispenser/6.
 % goalzone percept data plus MD (X,Y,MD)
:- dynamic mapGoalZone/3.
% rolezone percept data plus MD (X,Y,MD)
:- dynamic mapRoleZone/3.
% mapMarkExplored stored position of agents where has explored
:- dynamic mapMarkExplored/2.
% goalzone XY plus MD field (X,Y,MD)
:- dynamic targetClosestGoalZone/3.
% rolezone XY plus MD field (X,Y,MD)
:- dynamic targetClosestRoleZone/3.
% lowest and highest limit after which agent changes explore direction
:- dynamic limitChangeStepMinMax/2.
% step after which ACO is switched off
:- dynamic limitACOSteps/1.
% limits the tasks by remaining steps (Task1Limit,Task2Limit,Task3Limit,Task4Limit)
:- dynamic limitStepsTask/4.

% nearest Agent (Name, X, Y, MD).
:- dynamic targetNearestAgent/4.
% nearest Agent with needed block (Name, X, Y, MD).
:- dynamic targetNearestAgentWithNeededBlock/4.
% Message for offset calc (OwnX, OwnY, OtherX, OtherY, Step, AgentSender)
:- dynamic cachedSeenOtherAgentAt/6.
% message cached for offset
:- dynamic cachedMyOffsetOfOtherAgent/5.

% relative coordinates to other agents coordinate system (relX, relY, TheirName)
:- dynamic confirmedOffsetOfAgent/3.
% message to find relative coordinates between agents (myX, myY, seenX, seenY, SeenAtStep, MyName)
%:- dynamic seenOtherAgentAt/6.
% data for message (X,X, MyAgentName)
%:- dynamic sawGoalzoneAt/3.
% data for messages (X,Y, Type, MyAgentName)
%:- dynamic sawDispenserAt/4.

% Variables related to world measurement
% lists containing world X and Y sizes
:- dynamic worldListX/1, worldListY/1.
% how long does it take for a distStepNamePosition message to get processed
%(the greater the delay, the most sure we can be to have received all messages sent during a concrete step)
:- dynamic messageProcessingDelay/1.
% how long does a distStepNamePosition message still lingers around after being processed
:- dynamic messagePersitanceAfterDelay/1.
% Flags to steer world measurements.
:- dynamic worldUpdateX/0, worldUpdateY/0, worldUpdatedX/0, worldUpdatedY/0.
% store the size of the world on X and Y
:- dynamic worldSizeX/1, worldSizeY/1.

% messages can be commented out and still work; Variables related to goal zone and dispenser messaging
:- dynamic cachedMsgGoalZoneData/3.
:- dynamic cachedMsgRoleZoneData/3.
:- dynamic cachedMsgDispenserData/6.
% goalzone percept data plus sender name (X, Y, SenderName).
%:- dynamic messageGoalZone/3.
% goalzone percept data plus sender name (X, Y, SenderName).
%:- dynamic messageDeletedGoalZone/3.
% dispenser data plus sender name (X,Y,Type,Details,SenderName)
%:- dynamic messageDispenser/6.
% message containing just the sender name
%:- dynamic messageNeedGoalZone/1.
% message containing the requested dispenser details (BlockType) and the sender name
%:- dynamic messageNeedDispenser/2.

% Variables related to choosing or Determine Role
:- dynamic customRole/1. % role belief for non standard roles
:- dynamic targetRole/1.
:- dynamic randomSeed/1.
:- dynamic positionInHirarchie/1.
:- dynamic submitterLeader/0.

% coordinate supporting agents of submitterleaders
%(TaskType,Name1,X1,Y1,Block1,Pending1,Connected1,Name2,X2,Y2,Block2,Pending2,Connected2,Name3,X3,Y3,Block3,Pending3,
% Connected3)
:- dynamic multiTaskSupporterStatus/19.

% (SenderName, MsgStep, Role, Seed, SenderConnect, X, Y, BlockTypeAttached, TaskRole, [Energy])
:- dynamic storedOtherAgentStatus/9. 

% wait before chosing next task as submitterLeader
:- dynamic waitBeforeNewTask/1.

% coordination to avoid a norm or accept punishment
%(nameNorm,checkAvoid,stepFirst,stepLast,typeNorm,roleNorm,maxQuant,penaltyDmg,substituteOne,substituteTwo)
:- dynamic avoidNorm/10.
% Role Count for Norms
:- dynamic roleCount/1.
% After awakening
:- dynamic lastDeactivationTracker/1.

% Variables related to customRoleSaboteur role
:- dynamic emptyGoalZoneCounter/3. % Count of time no agents were seen around a specific goalZone.
:- dynamic searchInGoalzone/0. % switch to search for blokes next to goalzones
:- dynamic minimumDistanceEmptyGoalZone/1. % Distance between next goal zone to be visited if current empty
:- dynamic waitingTimeEmptyGoalZone/1. % Time to wait in an empty goal zone before moving to another one
%:- dynamic tempMapGoalZone/3. % Temporal variable to calculate next goal zone far enough of empty goal zones
:- dynamic getFree/0. % switch to detach other bloke
:- dynamic sabotageUs/0. % if set, the saboteur sabotages its own team members for testing goals
:- dynamic sabotageThem/0. % if set, the saboteur sabotages the members of other teams
:- dynamic whitelistedTeam/1. % holds the name of the team NOT to be sabotaged (us in competition against other teams).
:- dynamic visionRange/1. % stores the vision range of the agent.
:- dynamic agentSighting/8. % (X,Y,DirX,DirY,Step,Energy,BlockType,BlockDir) stores agent sightings, direction, step, supposed energy, carried block type, carried block direction.
:- dynamic maxEnergy/1. % stores the maximum energy of an agent.
:- dynamic recoverEnergy/1. % stores the energy recovered in a turn.
:- dynamic clearingTarget/2. % stores the target to clear next.
:- dynamic oldClearingTarget/2. % stores the last clearing target.
:- dynamic targetCandidate/5. % targetCandidate(X,Y,DirX,DirY,Energy), caches relevant information to select the next clearing target
:- dynamic maximumShootingRange/1. % maximum distance from which an agent should be shot at
:- dynamic inactiveSighting/3. % (X,Y,Timer) stores the coordinates of a bloke which is believed to be inactive and a timer.
%:- dynamic hitSighting/3. % (X,Y,Step) stores the coordinates of a bloke that has been hit as well as the step it was hit.
:- dynamic agentCounter/1. % counts possible agents seen
:- dynamic goalZoneSightings/5. % (X,Y,Number,Step,SenderName) information to be communicated by the saboteur by other agents if other agents seen in goalzone.
:- dynamic bodyCounter/1. % counts agents deactivated by the saboteur
:- dynamic saboteurHasMhTarget/0. % switch to decide manhattan target
:- dynamic submitterSighted/3. % (X,Y,Step) switch to decide manhattan target
:- dynamic inEmptyGoalZone/0. % switch to mark the saboteur is in an empty goalZone

% MACHINEL LEARNING VARs
:- dynamic qtable/6.
:- dynamic randomTaskChoser/1.

recoverEnergy(OldEnergy, RecoveredEnergy, NewEnergy) :-
	(NewEnergy is OldEnergy + RecoveredEnergy, NewEnergy =< 100);
	(NewEnergy is 100).
%trackDirection(X0,Y0,X1,Y1,DirX,DirY) :- DirX is X1-X0, DirY is Y1-Y0.
energyAfterDamage(Energy,X,Y,NewEnergy) :-
	(abs(X) + abs(Y) =:= 1, NewEnergy is Energy - 16, NewEnergy >= 0);
	(abs(X) + abs(Y) =:= 2, NewEnergy is Energy - 8, NewEnergy >= 0);
	(abs(X) + abs(Y) =:= 3, NewEnergy is Energy - 4, NewEnergy >= 0);
	(abs(X) + abs(Y) =:= 4, NewEnergy is Energy - 2, NewEnergy >= 0);
	(abs(X) + abs(Y) =:= 5, NewEnergy is Energy - 1, NewEnergy >= 0);
	(NewEnergy is 0).

% Transform XY coordinates concerning direction D nswe
transformXYD(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
transformXYD(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
transformXYD(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
transformXYD(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.

% Transform XY coordinates two times concerning direction D nswe
transformTwoTimesXYD(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 2.
transformTwoTimesXYD(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 2.
transformTwoTimesXYD(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 2.
transformTwoTimesXYD(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 2.

% Update position XY in relation to agent position X2 Y2
localize(X1, Y1, X2, Y2, X3, Y3) :- X3 is X1 + X2, Y3 is Y1 + Y2.

% Offset calculator
calculateAgentOffset(ReceiverBaseX, ReceiverBaseY, SenderBaseX, SenderBaseY, PerceptOffsetX, PerceptOffsetY,
		     OffsetX, OffsetY) :- OffsetX is ReceiverBaseX + PerceptOffsetX - SenderBaseX,
	             OffsetY is ReceiverBaseY + PerceptOffsetY - SenderBaseY .

% get random nswe direction
randomDirection(Dir) :- random_between(0, 3, D),
			integerToDirection(D, Dir).

% never go back with new random
randomGoForwardDirection(AltDir, NewDir) :- random_between(0, 3, D),
			integerToDirection(D, NewDir),
			oppositeDirection(AltDir, OppositAltDir),
			NewDir \= OppositAltDir.

% get a random number between 0 - 100
getRandomNumberSeed(RandomSeed) :- random_between (0, 10000, RandomSeed).

% skip random steps
skipRandomSteps(SkipSteps) :- random_between(0, 8, SkipSteps).

% get random 90 degree direction to initial affinity direction
random90Direction(Affini, AltDir) :- random_between(0, 1, RandD),
			flankingDirection(RandD, Affini, AltDir).

% get random rotate direction
randomRotate(Dir) :- random_between(0, 1, R),
			integerToRotate(R, Dir).

% Give random number between lower and higher bound
randomBetween(InLow, InHigh, RandOut) :- random_between(InLow, InHigh, RandOut).

% Calculate distance XY coordinates concerning target targetMd
calculateXYMd(X1, Y1, X2, Y2, Md) :- Md is abs(X1 - X2) + abs(Y1 - Y2).

% Calculate distance XY coordinates concerning target targetMd and taking into account the world size
%calculateXYMdWorldSize(X1, Y1, X2, Y2, SizeX, SizeY, Md) :-
%	(SizeX == 54321, SizeY == 54321, calculateXYMd(X1, Y1, X2, Y2, Md));
%	(SizeX \== 54321, SizeY \== 54321, absDistInMeasuredWorld(X1, X2, SizeX, DistanceX),
%	                                   absDistInMeasuredWorld(Y1, Y2, SizeY, DistanceY),
%	                                   Md is DistanceX + DistanceY).

% Calculate absolute distance between two points taking into account the size of the world
%absDistInMeasuredWorld(ObjectPos, AgentPos, WorldSize, Distance) :-
%    ( D1 is abs(ObjectPos - AgentPos),
%      D2 is abs(ObjectPos + WorldSize - AgentPos),
%     D3 is abs(AgentPos - ObjectPos),
%     D4 is abs(AgentPos + WorldSize - ObjectPos),
%     min_list([D1,D2,D3,D4], MinD), Distance is MinD ).

% calculate minus or plus 1
calculateMinusOne(A1, A2) :- A2 is (A1 - 1).
calculatePlusOne(B1, B2) :- B2 is (B1 + 1).

% return coordinates in front of attached block concerning move direction
% (DirMoving, BlockAttach, FrontBlockX, FrontBlockY)
clearAttachedDirection(n, w, -1, -1).
clearAttachedDirection(n, e, 1, -1).
clearAttachedDirection(n, n, 0, -2).
clearAttachedDirection(n, s, 0, -1).
clearAttachedDirection(s, w, -1, 1).
clearAttachedDirection(s, e, 1, 1).
clearAttachedDirection(s, s, 0, 2).
clearAttachedDirection(s, n, 0, 1).
clearAttachedDirection(w, s, -1, 1).
clearAttachedDirection(w, n, -1, -1).
clearAttachedDirection(w, w, -2, 0).
clearAttachedDirection(w, e, -1, 0).
clearAttachedDirection(e, s, 1, 1).
clearAttachedDirection(e, n, 1, -1).
clearAttachedDirection(e, e, 2, 0).
clearAttachedDirection(e, w, 1, 0).

% diagonal coords of direction blocks move with agent concerning DirAffi and 90degreeDir
diagonalCoordForAffiAndOppoBlock(n,e,1,1).
diagonalCoordForAffiAndOppoBlock(n,w,-1,1).
diagonalCoordForAffiAndOppoBlock(s,w,-1,-1).
diagonalCoordForAffiAndOppoBlock(s,e,1,-1).
diagonalCoordForAffiAndOppoBlock(w,n,1,-1).
diagonalCoordForAffiAndOppoBlock(w,s,1,1).
diagonalCoordForAffiAndOppoBlock(e,s,-1,1).
diagonalCoordForAffiAndOppoBlock(e,n,-1,-1).

% helper function integer to rotate
integerToRotate(0, cw).
integerToRotate(1, ccw).

% helper function random to direction
integerToDirection(0, n).
integerToDirection(1, s).
integerToDirection(2, w).
integerToDirection(3, e).

% helper function random to 90 degree direction
flankingDirection(0, n, w).
flankingDirection(1, n, e).
flankingDirection(0, s, w).
flankingDirection(1, s, e).
flankingDirection(0, e, s).
flankingDirection(1, e, n).
flankingDirection(0, w, s).
flankingDirection(1, w, n).

% helper function direction to coordinate relative to agent
directionToCoordinate(n, 0, -1).
directionToCoordinate(s, 0, 1).
directionToCoordinate(w, -1, 0).
directionToCoordinate(e, 1, 0).

% helper function number to roles
%numbertoRoles(0, worker).
%numbertoRoles(1, explorer).

% helper function direction to opposite direction
oppositeDirection(n, s).
oppositeDirection(s, n).
oppositeDirection(w, e).
oppositeDirection(e, w).

% helper function direction to opposite rotate
oppositeRotate(cw, ccw).
oppositeRotate(ccw, cw).

% helper function 2 directions to rotation (DirMove, DirBlock, Rotate)
rotateAgainstAffinity(n, n, cw).
rotateAgainstAffinity(n, w, ccw).
rotateAgainstAffinity(n, e, cw).
rotateAgainstAffinity(w, n, cw).
rotateAgainstAffinity(w, s, ccw).
rotateAgainstAffinity(w, w, ccw).
rotateAgainstAffinity(s, s, cw).
rotateAgainstAffinity(s, w, cw).
rotateAgainstAffinity(s, e, ccw).
rotateAgainstAffinity(e, e, cw).
rotateAgainstAffinity(e, n, ccw).
rotateAgainstAffinity(e, s, cw).

% helper function rotation to nswe (, Rotate, DirTarget)
rotateToDirection(n, cw, e).
rotateToDirection(n, ccw, w).
rotateToDirection(s, cw, w).
rotateToDirection(s, ccw, e).
rotateToDirection(w, cw, n).
rotateToDirection(w, ccw, s).
rotateToDirection(e, cw, s).
rotateToDirection(e, ccw, n).

% helper function rotation to coordinate nswe (DirBlock, Rotate, RotTargetX, RotTargetY)
rotateToCoord(n, cw, 1, 0).
rotateToCoord(n, ccw, -1, 0).
rotateToCoord(s, cw, -1, 0).
rotateToCoord(s, ccw, 1, 0).
rotateToCoord(w, cw, 0, -1).
rotateToCoord(w, ccw, 0, 1).
rotateToCoord(e, cw, 0, 1).
rotateToCoord(e, ccw, 0, -1).

% check supportingAgent block coordinates for wait/immediate delivery
checkBlockPosition(-1, 1, alwaysDeliver).
checkBlockPosition(-2, 1, alwaysDeliver).
checkBlockPosition(-3, 1, alwaysDeliver).
checkBlockPosition(1, 1, alwaysDeliver).
checkBlockPosition(2, 1, alwaysDeliver).
checkBlockPosition(3, 1, alwaysDeliver).

checkBlockPosition(0, 2, alwaysDeliver).
checkBlockPosition(0, 3, wait).
checkBlockPosition(0, 4, wait).

checkBlockPosition(-3, 2, wait).
checkBlockPosition(-2, 2, wait).
checkBlockPosition(-1, 2, wait).
checkBlockPosition(1, 2, wait).
checkBlockPosition(2, 2, wait).
checkBlockPosition(3, 2, wait).
checkBlockPosition(-3, 3, wait).
checkBlockPosition(-2, 3, wait).
checkBlockPosition(-1, 3, wait).
checkBlockPosition(1, 3, wait).
checkBlockPosition(2, 3, wait).
checkBlockPosition(3, 3, wait).
checkBlockPosition(-3, 4, wait).
checkBlockPosition(-2, 4, wait).
checkBlockPosition(-1, 4, wait).
checkBlockPosition(1, 4, wait).
checkBlockPosition(2, 4, wait).
checkBlockPosition(3, 4, wait).

% Gives the apparent displacement of the observed agent relative to the observing agent between t0 and t1.
%distanceBetweenPoints(X1, Y1, X2, Y2, DistX, DistY) :- DistX is X1 - X2, DistY is Y1 - Y2.

% modulo function for 2 values. It only returns positive values.
%getModPos(X, Y, Z) :- (X >= 0, Z is X mod Y); (X < 0, V is abs(Y+X), Z is V mod Y).

%
%getModPos(X1,Y1,SizeX,SizeY,X2,Y2) :-
%    ((SizeX == 1000, SizeY == 1000), X2 is X1, Y2 is Y1);
%    (getModPos(X1,SizeX,X2), getModPos(Y1,SizeY,Y2)).

% modulo function for 2 values. It returns int values.
%getModInt(X, Y, Z) :- (X >= 0, Z is X mod Y); (X < 0, Z is X mod -Y).

% ToDo worldsize reactivate
% conditional modulo function: it only triggers if the world size is no longer 1000, it returns pos values
%getModInt(X1,Y1,SizeX,SizeY,X2,Y2) :-
%    ((SizeX == 1000, SizeY == 1000), X2 is X1, Y2 is Y1);
%    (getModInt(X1,SizeX,X2), getModInt(Y1,SizeY,Y2)).

% calculates the distance between an object and an agent according to the agent perception limits and the size of the world
%getPerceivedDistance(ObjectAt, AgentAt, WorldSize, PerceptDistance, Z) :-
%	(Delta is ObjectAt - AgentAt), ((   Delta >= 0, Delta =< PerceptDistance, Z is Delta );
%                       (   Delta >= 0, Delta > PerceptDistance, V is Delta - WorldSize, ((abs(V) < abs(Delta), Z is V); (abs(V) >= abs(Delta), Z is Delta)));
%                       (   Delta < 0, abs(Delta) =< PerceptDistance, Z is Delta);
%                       (   Delta < 0, abs(Delta) > PerceptDistance, getModPos(Delta, WorldSize, V), ((abs(V) < abs(Delta), Z is V); (abs(V) >= abs(Delta), Z is Delta)))).

% world size calculator
%getWorldSize(PosDiff, OldWorldSize, NewWorldSize) :-
%	(abs(PosDiff) > 0, abs(PosDiff) < OldWorldSize, OldWorldSize - abs(PosDiff) > abs(PosDiff), NewWorldSize is abs(PosDiff));
%	(abs(PosDiff) > 0, abs(PosDiff) < OldWorldSize, OldWorldSize - abs(PosDiff) < abs(PosDiff), NewWorldSize is OldWorldSize - abs(PosDiff));
%	(PosDiff = 0, NewWorldSize is OldWorldSize);
%	(abs(PosDiff) >= OldWorldSize, NewWorldSize is OldWorldSize).

% Counting the quantity of repeated world measurement in the world measurement list.
count_repeated([Elem|Xs], Elem, Count, Ys) :- count_repeated(Xs, Elem, Count1, Ys), Count is Count1+1.
count_repeated([AnotherElem|Ys], Elem, 0, [AnotherElem|Ys]) :- Elem \= AnotherElem.
count_repeated([], _, 0, []).

rle([X|Xs], [[C,X]|Ys]) :- count_repeated([X|Xs], X, C, Zs), rle(Zs, Ys).
rle([], []).
