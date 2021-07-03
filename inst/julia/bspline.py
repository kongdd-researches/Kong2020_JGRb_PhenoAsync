def B_spline(p_list):
    """
    :param p_list: (list of list of int:[[x0, y0], [x1, y1], ...]) point set of p
    result: (list of list of int:[[x0, y0], [x1, y1], ...]) point on curve
    绘制三次(四阶)均匀B样条曲线
    """
    result = []
    n = len(p_list)
    k = 4
    u = k-1
    while (u < n+1):
        x, y = 0, 0
        #calc P(u)
        for i in range(0, n):
            B_ik = deBoor_Cox(u, k, i)
            x += B_ik * p_list[i][0]
            y += B_ik * p_list[i][1]
        result.append((int(x+0.5), int(y+0.5)))
        u += 1 #2020/09/27
    return result

def deBoor_Cox(u, k, i):
    if k==1:
        if i <= u and u <= i+1:
            return 1
        else:
            return 0
    else:
        coef_1, coef_2 = 0, 0
        if (u-i == 0) and (i+k-1-i == 0):
            coef_1 = 0
        else:
            coef_1 = (u-i) / (i+k-1-i)
        if (i+k-u == 0) and (i+k-i-1 == 0)
            coef_2 = 0
        else:
            coef_2 = (i+k-u) / (i+k-i-1)
    return coef_1 * deBoor_Cox(u, k-1, i) + coef_2 * deBoox_Cox(u, k-1, i+1)
