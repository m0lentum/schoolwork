STEP_LENGTH = 100000;
STEP_COUNT = 3;
DT = 3600;
TIMESTEP_COUNT = 4;

u = [5.3 4.5 3.9 1.3 3.2 5.2 4.9 3.9 3.9];
v = [0 0 0 2.4 2.0 1.2 0 0 0];
T0 = [5 10 15 5 10 15 5 10 15];

uCoefs = (diag(u + v, 0) ...
          - diag(u(2:end) .* [1 1 0 1 1 0 1 1], -1)...
          - diag(v(4:end), -3))...
          / STEP_LENGTH;

rhs = [(u(1) * T0(1)) 0 0 (u(4) * T0(4)) 0 0 (u(7) * T0(7)) 0 0]'...
      / STEP_LENGTH;

result = zeros(9, TIMESTEP_COUNT);
result(:,1) = T0;
for ti = 2 : TIMESTEP_COUNT
  result(:,ti) = result(:,ti-1) + DT * (-uCoefs * result(:,ti-1) + rhs);
end

% piirto

% muokataan ratkaisu 3D-matriisiksi jossa x-, y- ja t-ulottuvuudet
resultMats = zeros(STEP_COUNT, STEP_COUNT, TIMESTEP_COUNT);
for ti = 1 : TIMESTEP_COUNT
  for xi = 1 : STEP_COUNT
    for yi = 1 : STEP_COUNT
      resultMats(yi, xi, ti) = result((yi-1) * STEP_COUNT + xi, ti);
    end
  end
end

x = STEP_LENGTH : STEP_LENGTH : 3 * STEP_LENGTH;
y = STEP_LENGTH : STEP_LENGTH : 3 * STEP_LENGTH;
figure
s = surf(x, y, resultMats(:,:,1));

for ti = 1 : TIMESTEP_COUNT
  xlabel('x')
  ylabel('y')
  zlabel('u')
  set(s, 'ZData', resultMats(:,:,ti));
  pause(1.0);
end

