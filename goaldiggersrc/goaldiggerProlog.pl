/**
 * Prolog Knowledge Base of Agent
 *
 */

:- dynamic stepAwaitingAction/1, haveMove/1, step/1, elapseStepTime/1, lDebugOn/1.
:- dynamic agentAt/2, thing/4, randomAffinity/1. % thing equals thing from percept
:- dynamic targetMd/2, nMd/1, sMd/1, wMd/1, eMd/1, executeManhattan/1. % Variables for Manhatten Distance 
:- dynamic haveBlockAttached/2. % (Bool, Dir)
:- dynamic haveDispenserDelivery/2. % switch dispenser delivered block (Bool, Step)
:- dynamic targetDispenserAt/4. % (X,Y,BlockType,MD) of dispenser to search out for
:- dynamic targetClosestOfAllDispensersAt/4. % closest of all known dispensers (X,Y,Blocktype, MD)
:- dynamic skipThisStep/1. % do skip/explore until this step
:- dynamic changeAffinityAfterTheseSteps/1. % as told changes random affinity
:- dynamic currentChosenTask/6. % task the agent has chosen and works on (TaskName, TaskStep, Reward, X, Y, BlockType)
:- dynamic thingDispenser/5. % dispenser percept data plus MD (X,Y,Type,Details,MD)
:- dynamic storedGoalZone/3. % goalzone percept data plus MD (X,Y,MD)
:- dynamic targetClosestGoalZone/3. % goalzone XY plus MD field (X,Y,MD)
:- dynamic activateDoubleSpeed/1.  % switch to deactivate double speed for workers
:- dynamic seenOtherAgentAt/6. % message to find relative coordinates between agents (myX, myY, seenX, seenY, SeenAtStep, MyName)
:- dynamic confirmedOtherAgentAt/3. % relative coordinates to other agents coordinate system (relX, relY, TheirName)
:- dynamic sawGoalzoneAt/3. % data for message (X,X, MyAgentName)
:- dynamic sawDispenserAt/4. % data for messages (X,Y, Type, MyAgentName)


% Transform XY coordinates concerning direction D nswe
transformXYD(n, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 - 1.
transformXYD(s, X1, Y1, X2, Y2) :- X2 = X1, Y2 is Y1 + 1.
transformXYD(e, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 + 1.
transformXYD(w, X1, Y1, X2, Y2) :- Y2 = Y1, X2 is X1 - 1.

% Update position XY in relation to agent position X2 Y2
localize(X1, Y1, X2, Y2, X3, Y3) :- X3 is X1 + X2, Y3 is Y1 + Y2.
delocalize(X1, Y1, X2, Y2, X3, Y3) :- X3 is X1 + (X2 * -1), Y3 is Y1 + (Y2 * -1). % X1Y1 gets localized by X2Y2 (add negative values, substract positive values)

% get random nswe direction
randomDirection(Dir) :- random_between(0, 3, D),
			integerToDirection(D, Dir).
			
% skip random steps
skipRandomSteps(SkipSteps) :- random_between(2, 6, SkipSteps).
			
% get random 90 degree direction to initial affinity direction		
random90Direction(Affini, AltDir) :- random_between(0, 1, RandD),
			flankingDirection(RandD, Affini, AltDir).

% get random role ToDo STILL NECESSARY???
%randomRole(Role) :- random_between(0, 1, RD), numbertoRoles(RD, Role).

% get random rotate direction
randomRotate(Dir) :- random_between(0, 1, R),
			integerToRotate(R, Dir).

% Give random number for steps after which to change affinity			
randomChangeStep(Rand) :- random_between(30, 60, Rand).

% Calculate distance XY coordinates concerning target targetMd
calculateXYMd(X1, Y1, X2, Y2, Md) :- Md is abs(X1 - X2) + abs(Y1 - Y2).

% calculate minus or plus 1
calculateMinusOne(A1, A2) :- A2 is (A1 - 1).
calculatePlusOne(B1, B2) :- B2 is (B1 + 1).

% return coordinates in front of attached block concerning move direction
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

% helper function 2 directions to rotation
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

% helper function rotation to nswe
rotateToDirection(n, cw, e).
rotateToDirection(n, ccw, w).
rotateToDirection(s, cw, w).
rotateToDirection(s, ccw, e).
rotateToDirection(w, cw, n).
rotateToDirection(w, ccw, s).
rotateToDirection(e, cw, s).
rotateToDirection(e, ccw, n).

% helper function rotation to coordinate nswe
rotateToCoord(n, cw, 1, 0).
rotateToCoord(n, ccw, -1, 0).
rotateToCoord(s, cw, -1, 0).
rotateToCoord(s, ccw, 1, 0).
rotateToCoord(w, cw, 0, -1).
rotateToCoord(w, ccw, 0, 1).
rotateToCoord(e, cw, 0, 1).
rotateToCoord(e, ccw, 0, -1).



