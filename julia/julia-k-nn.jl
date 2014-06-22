import Base: sumabs2, LinAlg.BLAS.gemm!
 
function knn(tfile, vfile)
    (tdata, theader) = readcsv(tfile, Uint8, header = true) # if this errors, replace header with has_header
    (vdata, vheader) = readcsv(vfile, Uint8, header = true)
    tlabels = tdata[:, 1]
    vlabels = vdata[:, 1]
    nt = length(tlabels)
    nv = length(vlabels)
    # convert to float32 to use sgemm from BLAS
    T = float32(tdata[:, 2:end])
    V = float32(vdata[:, 2:end])
 
    # use trick from julialang.org/blog/2013/09/fast-numeric
    # sum(abs(t-v)^2) = sum(abs(t)^2) + sum(abs(v)^2) - 2*dot(t,v)
    st = sumabs2(T, 2)
    sv = sumabs2(V, 2)
    # broadcasting sum of column .+ row => matrix
    R = st .+ reshape(sv, 1, nv)
    α :: Float32 = -2.0
    β :: Float32 = 1.0
    # BLAS gemm, update R in-place to α*T*V' + β*R
    gemm!('N', 'T', α, T, V, β, R)
    (minval, index) = findmin(R, 1)
    num_correct = 0
    for i = 1:nv
        if tlabels[mod1(index[i], nt)] == vlabels[i]
            num_correct += 1
        end
    end
    println("Percentage correct: ", 100*num_correct/nv, "%")
end
 
# don't time the first run - otherwise should include compile time for static languages
knn("trainingsample.csv", "validationsample.csv")
@time knn("trainingsample.csv", "validationsample.csv")
@time knn("trainingsample.csv", "validationsample.csv")
@time knn("trainingsample.csv", "validationsample.csv")
