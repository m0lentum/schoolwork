f = @(x) cos(x) - x;

left_bound = 0.5;
right_bound = 1.0;

if f(left_bound) * f(right_bound) > 0
  error("Initial guess did not contain root");
end

for i = 1 : 5
  center = (left_bound + right_bound) / 2.0;
  if sign(f(center)) != sign(f(left_bound))
    right_bound = center;
  else
    left_bound = center;
  end
end

ans = (left_bound + right_bound) / 2.0
