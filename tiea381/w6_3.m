% yhtälön oikean puolen funktio vektorimuodossa
f = @(theta) [theta(2), -0.1*theta(2) - sin(theta(1))];
h = 0.1;
t = 0 : h : 10;
step_count = length(t);
% riippuva muuttuja myös vektorimuodossa [theta_0, theta_1]
theta = zeros(step_count,2);
theta(1,1) = 0.5;
theta(2,1) = 0;

for step = 1 : step_count-1
  theta_n = theta(step,:);
  k1 = h*f(theta_n);
  k2 = h*f(theta_n + k1/2);
  k3 = h*f(theta_n + k2/2);
  k4 = h*f(theta_n + k3);
  theta(step+1,:) = theta_n + k1/6 + k2/3 + k3/3 + k4/6;
end

% theta(1) on haluttu funktio, theta(2) sen derivaatta
plot(t, theta(:,1))
xlabel('t');
ylabel('theta');

