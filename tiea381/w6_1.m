f = @(y) -y^2;

h = 1/2;
t = 0 : h : 2;
step_count = length(t);
y = zeros(step_count,1);
y(1) = 1;

for step = 1 : step_count-1
  euler_guess = y(step) + h * f(y(step));
  y(step+1) = y(step) + h * f(euler_guess);
end

fprintf('y(2) = %.6g\n', y(end));
