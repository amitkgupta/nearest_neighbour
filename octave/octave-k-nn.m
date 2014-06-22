tic
T = csvread('trainingsample.csv', 1, 0);
V = csvread('validationsample.csv', 1, 0);
tlabels = T(:, 1);
vlabels = V(:, 1);
nt = length(tlabels);
nv = length(vlabels);
T = T(:, 2:end);
V = V(:, 2:end);
st = sum(T.^2, 2);
sv = sum(V.^2, 2);
R = repmat(st, 1, nv) + repmat(sv', nt, 1) - 2*T*V';
[minval, index] = min(R, [], 1);
fprintf('Percentage correct: %f%%\n', 100*mean(tlabels(index) == vlabels))
toc

