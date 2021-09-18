alpha = sqrt(2) / 2;
A = zeros(13, 13);
b = zeros(13, 1);
A(1,2) = 1; A(1,6) = -1;
A(2,3) = 1; b(2) = 10;
A(3,1) = alpha; A(3,4) = -1; A(3,5) = -alpha;
A(4,1) = alpha; A(4,3) = 1; A(4,5) = alpha;
A(5,4) = 1; A(5,8) = -1;
A(6,7) = 1;
A(7,5) = alpha; A(7,6) = 1; A(7,9) = -alpha; A(7,10) = -1;
A(8,5) = alpha; A(8,7) = 1; A(8,9) = alpha; b(8) = 15;
A(9,10) = 1; A(9,13) = -1;
A(10,11) = 1; b(10) = 20;
A(11,8) = 1; A(11,9) = alpha; A(11,12) = -alpha;
A(12,9) = alpha; A(12,11) = 1; A(12,12) = alpha;
A(13,13) = 1; A(13,12) = alpha;

% ratkaistaan x yhtälöstä Ax = b
x = A\b
