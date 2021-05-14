function L = w7_1_L(pointCount)
  L = diag(2 * ones(pointCount, 1)) ...
    - diag(ones(pointCount - 1, 1), 1) ...
    - diag(ones(pointCount - 1, 1), -1);
end
