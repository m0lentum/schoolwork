function result = exam_1_solve(stepCount, timeStepLength, timeStepCount)
  % laskentapisteet alueen sisällä, pisteitä 1 vähemmän kuin jakovälejä
  pointCount = stepCount - 1;
  stepLength = 1 / stepCount;
  stepInvSq = 1 / stepLength^2;
  A = stepInvSq * (diag(2 * ones(pointCount, 1)) ...
    + diag(-1 * ones(pointCount - 1, 1), 1) ...
    + diag(-1 * ones(pointCount - 1, 1), -1));
  % u^(k):n kerroinmatriisi aika-askelluksessa
  timeStepMat = inv(eye(pointCount) + 0.5 * timeStepLength * A) ...
    * (eye(pointCount) - 0.5 * timeStepLength * A);

  % ratkaisu alueen sisäpisteissä
  x = 1+stepLength : stepLength : 2-stepLength;
  initialState = 8 .* (x - 1) .* (2 - x);
  % ei tarvita kaikkien aika-askelten tilaa,
  % joten pidetään muistissa vain viimeinen
  result = transpose(initialState);
  for ti = 1 : timeStepCount
    result = timeStepMat * result;
  end
end
