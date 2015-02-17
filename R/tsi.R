#This function is used to compute the measures of tournament stability
#input is a matrix, and number of matches between each of two teams
#output is a list, rank is the rank computed by alway algroithm
#t_o is tau O and t_r is tau R; t_o_hat is predicted tau O and t_r_hat is
#predited tau R

tsi<-function(M,n){
    m <- nrow(M)
    rank <- alway(M)
    rank1 <- rank1(M)
    Mo <- matrix_change(M,rank)
    Mo_wl <- change2(Mo)
    Mo_wl <- Mo_wl*Mo
    Mr <- matrix_change(M,rank1)
    Mr_wl <- change2(Mr)
    Mr_wl <- Mr_wl*Mr
    total = sum(Mo)
    t_o = sum(Mo_wl)/total
    t_r = sum(Mr_wl)/total
    e_o = find_eo(m,n)
    e_r = find_er(m,n)
    t_o_hat = (t_o-e_o)/(1-e_o)
    t_r_hat = (t_r-e_r)/(1-e_r)
    result = list(rank = rank, t_r = t_r, t_o = t_o, t_r_hat = t_r_hat, t_o_hat = t_o_hat)
    return(result)
}

#This function is used to make a new matrix based on the rank
#input is a matrix, and a rank
#output is a new matrix
matrix_change<-function(M,sequence){
    n=ncol(M)
    new_matrix<-NULL
    for(i in 1:n){
        temp<-M[sequence[i],]
        temp<-temp[sequence]
        new_matrix<-rbind(new_matrix,temp)
    }
    return(new_matrix)
}

#This function is used to translate matrix to win-loss formate
#input is a matrix
#output is a win-loss formate matrix
change2<-function(M){
    m <- M
    m[upper.tri(m)] = 1
    m[lower.tri(m)] = -1
    return(m)
}

#This function is used to calculate rank by alway's algorithm
#input is a matrix
#output is a rank by alway's algorithm

alway <- function(m){
    n <- nrow(m)
    rank = rep(1:n)
    new_m <- m - t(m)
    m <- new_m
    condition1 = TRUE
    while (condition1){
        condition1 = FALSE
        condition2 = TRUE
        i = 1
        while(condition2){
            temp = 0
            j = i
            condition3 = TRUE
            while(condition3){
                temp = temp + new_m[i,j]
                if(temp<0){
                    condition1 = TRUE
                    temp2 = rank[j]
                    rank[j] = rank[i]
                    rank[i] = temp2
                    new_m = matrix_change(m, rank)
                    condition3 = FALSE
                    condition2 = FALSE

                }
                j = j + 1
                if (j == n+1){
                    condition3 = FALSE
                }
            }
            i = i + 1
            if (i==n){
                condition2 = FALSE
            }
        }
    }
    return(rank)

}

#This function is used to calculate the expected tau r
#input is a number of teams and number of matches between each teams
#output is a expected tau r


find_er <- function(m,n){
    er_data = c(1.000,0.000,0.501,0.250,0.498,0.83,0.372,0.11,0.377,0.056,0.842,0.085,0.473,0.077,0.417,0.047,0.348,0.037,0.316,0.028,0.791,0.026,0.458,0.036,0.381,0.021,0.314,0.018,0.286,0.014,0.713,0.032,0.409,0.023,0.343,0.014,0.283,0.011,0.259,0.0088,0.642,0.024,0.381,0.016,0.318,0.0098,0.262,0.0077,0.236,0.0062,0.596,0.019,0.352,0.012,0.294,0.0073,0.243,0.0060,0.219,0.0045,0.552,0.015,0.331,0.0090,0.276,0.0052,0.225,0.0045,0.206,0.0036,0.521,0.012,0.310,0.0071,0.261,0.0043,0.215,0.0036,0.193,0.0027,0.489,0.0092,0.295,0.0057,0.246,0.0035,0.204,0.0028,0.183,0.0022,0.467,0.0078,0.281,0.0048,0.234,0.0029,0.194,0.0024,0.175,0.0018,0.445,0.0068,0.270,0.0041,0.225,0.0025,0.184,0.0021,0.169,0.0016,0.426,0.0059,0.259,0.0036,0.216,0.0021,0.178,0.0018,0.161,0.0013,0.409,0.0048,0.250,0.0031,0.206,0.0019,0.171,0.0015,0.156,0.0011,0.393,0.0046,0.240,0.0027,0.201,0.0017,0.166,0.0013,0.149,0.0010,0.380,0.0040,0.233,0.0023,0.195,0.0014,0.160,0.0011,0.145,0.0009,0.368,0.0036,0.224,0.0020,0.187,0.0012,0.155,0.0010,0.141,0.0008,0.356,0.0031,0.218,0.0019,0.183,0.0011,0.151,0.0009,0.137,0.0007,0.345,0.0028,0.211,0.0017,0.178,0.0010,0.147,0.0008,0.133,0.0006,0.336,0.0026,0.207,0.0015,0.173,0.0009,0.144,0.0007,0.130,0.0006,0.327,0.0023,0.201,0.0014,0.169,0.0008,0.140,0.0007,0.127,0.0005,0.319,0.0021,0.198,0.0013,0.166,0.0008,0.136,0.0006,0.123,0.0005,0.310,0.0021,0.193,0.0012,0.161,0.0007,0.133,0.0006,0.121,0.0004,0.305,0.0018,0.187,0.0011,0.158,0.0007,0.130,0.0005,0.118,0.0004,0.298,0.0017,0.185,0.0010,0.154,0.0006,0.127,0.0005,0.116,0.0004,0.290,0.0016,0.180,0.0009,0.151,0.0006,0.126,0.0004,0.113,0.0003,0.286,0.0014,0.176,0.0008,0.149,0.0005,0.122,0.0004,0.111,0.0003,0.280,0.0014,0.173,0.0008,0.145,0.0005,0.120,0.0004,0.109,0.0003,0.274,0.0013,0.169,0.0007,0.143,0.0005,0.118,0.0003,0.107,0.0003,0.269,0.0012,0.167,0.0007,0.141,0.0004,0.116,0.0003,0.106,0.0003)
    er_table = matrix(er_data, ncol = 10, nrow = 29, byrow = TRUE)
    er_mean_table = er_table[,c(1,3,5,7,9)]
    er_var_table = er_table[,c(2,4,6,8,10)]
    m = m-1
    return(er_mean_table[m,n])
}

#This function is used to calculate the expected tau o
#input is a number of teams and number of matches between each teams
#output is a expected tau o

find_eo <- function(m,n){
    eo_data = c(1.000,0.000,0.510,0.250,0.505,0.085,0.376,0.110,0.376,0.060,0.828,0.085,0.474,0.075,0.445,0.037,0.354,0.034,0.342,0.022,0.789,0.026,0.458,0.035,0.413,0.017,0.336,0.016,0.316,0.011,0.730,0.024,0.443,0.020,0.386,0.011,0.317,0.0090,0.297,0.0069,0.685,0.016,0.420,0.013,0.368,0.0073,0.304,0.0059,0.279,0.0047,0.648,0.013,0.401,0.0091,0.349,0.0053,0.289,0.0044,0.268,0.0034,0.618,0.010,0.383,0.0068,0.334,0.0042,0.277,0.0034,0.255,0.0025,0.591,0.0081,0.371,0.0053,0.320,0.0033,0.268,0.0026,0.246,0.0021,0.567,0.0067,0.357,0.0044,0.310,0.0025,0.258,0.0021,0.237,0.0017,0.547,0.0055,0.347,0.0035,0.299,0.0022,0.249,0.0017,0.228,0.0014,0.530,0.0048,0.338,0.0029,0.289,0.0018,0.242,0.0014,0.222,0.0011,0.513,0.0043,0.328,0.0025,0.281,0.0016,0.235,0.0012,0.215,0.0010,0.496,0.0038,0.320,0.0022,0.274,0.0013,0.229,0.0010,0.210,0.0008,0.482,0.0032,0.312,0.0019,0.266,0.0012,0.224,0.0009,0.204,0.0007,0.470,0.0029,0.304,0.0016,0.260,0.0011,0.219,0.0008,0.199,0.0006,0.459,0.0025,0.298,0.0014,0.253,0.0010,0.213,0.0007,0.194,0.0006,0.449,0.0023,0.292,0.0012,0.248,0.0008,0.209,0.0006,0.191,0.0005,0.439,0.0020,0.286,0.0012,0.243,0.0007,0.205,0.0006,0.187,0.0004,0.429,0.0019,0.281,0.0010,0.238,0.0007,0.202,0.0005,0.183,0.0004,0.421,0.0017,0.276,0.0009,0.234,0.0006,0.198,0.0005,0.180,0.0004,0.415,0.0015,0.272,0.0008,0.229,0.0005,0.194,0.0004,0.177,0.0003,0.406,0.0014,0.268,0.0008,0.226,0.0005,0.191,0.0004,0.174,0.0003,0.399,0.0013,0.263,0.0007,0.222,0.0004,0.188,0.0003,0.171,0.0003,0.392,0.0012,0.260,0.0006,0.218,0.0004,0.186,0.0003,0.168,0.0003,0.386,0.0011,0.256,0.0006,0.215,0.0004,0.183,0.0003,0.166,0.0002,0.380,0.0011,0.252,0.0005,0.212,0.0004,0.180,0.0003,0.163,0.0002,0.375,0.0010,0.249,0.0005,0.209,0.0003,0.178,0.0003,0.161,0.0002,0.370,0.0009,0.245,0.0005,0.206,0.0003,0.176,0.0002,0.159,0.0002,0.364,0.0008,0.242,0.0004,0.203,0.0003,0.173,0.0002,0.156,0.0002)
    eo_table = matrix(eo_data, ncol = 10, nrow = 29, byrow = TRUE)
    eo_mean_table = eo_table[,c(1,3,5,7,9)]
    eo_var_table = eo_table[,c(2,4,6,8,10)]
    m = m-1
    return(eo_mean_table[m,n])
}

#This function is used to calculate the expected tau r for large team number or large matches number
#input is a number of teams and number of matches between each teams
#output is a expected tau r
find_er_large <- function(m,n,n_tries){
    sum = 0
    for (i in 1:n_tries){
        M = matrix(sample(n,m*m,replace=TRUE),m,m)
        M[lower.tri(M)] = 0
        M2 = n-t(M)
        M2[upper.tri(M2)] = 0
        Mk = M+M2
        diag(Mk) <- 0
        Mr <- Mk
        Mr_wl <- change2(Mr)
        Mr_wl <- Mr_wl*Mr
        total = sum(Mr)
        t_r = sum(Mr_wl)/total
        sum = t_r + sum
    }
    result = sum/n_tries
    return(result)
}

#This function is used to calculate the expected tau o for large team number or large matches number
#input is a number of teams and number of matches between each teams
#output is a expected tau o
find_eo_large <- function(m,n,n_tries){
    sum = 0
    for (i in 1:n_tries){
        M = matrix(sample(n,m*m,replace=TRUE),m,m)
        M[lower.tri(M)] = 0
        M2 = n-t(M)
        M2[upper.tri(M2)] = 0
        Mk = M+M2
        diag(Mk) <- 0
        rank <- alway(Mk)
        Mr <- matrix_change(Mk,rank)
        Mr_wl <- change2(Mr)
        Mr_wl <- Mr_wl*Mr
        total = sum(Mr)
        t_r = sum(Mr_wl)/total
        sum = t_r + sum
    }
    result = sum/n_tries
    return(result)
}

rank1 <- function(M){
    n <- nrow(M)
    k = rowSums(M)
    p <- matrix(c(c(1:n),k), nrow = 2, ncol = n, byrow = TRUE)
    for(i in 2:n){
        for (j in 1:i){
            if (p[2,i] > p[2,j]){
                temp = p[,i]
                p[,i] = p[,j]
                p[,j] = temp
            }
        }
    }
    print(p)
    return(p[1,])

}
