f = @(y) 6.22e-19 * (2e3 - y/2)^2 * (2e3 - y/2)^2 * (3e3 - 3*y/4)^2;

h = 0.01;
t = 0 : h : 0.2;
step_count = length(t);
y = zeros(step_count,1);
y(1) = 0;

for step = 1 : step_count-1
  euler_guess = y(step) + h * f(y(step));
  y(step+1) = y(step) + h * f(euler_guess);
end

fprintf('y(0.2) = %.6g\n', y(end));
