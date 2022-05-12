t = 20
a = 10
b = 10
a_yes = 7
b_yes = 4
t_yes = a_yes + b_yes
t_no = t - t_yes
a_yes_pc = 100*a_yes/a
b_yes_pc = 100*b_yes/b
ab_yes_pc = a_yes_pc-b_yes_pc
cat('observed yes rate (%): A:', a_yes_pc,
           ', B:', b_yes_pc, ', A-B:', ab_yes_pc,
           '\nTotal counts: yes:', t_yes,
           ',No:', t_no)

set.seed(1)
bag1 = c(rep(1, t_yes), rep(0, t_no))
p = 100
prem_res = rep(0,p)
for (i in 1:p) {
     bag = sample(bag1)
     a_rs = bag[1:a]
     b_rs = bag[(a+1):(a+b)]
     prem_res[i] = 100*sum(a_rs)/a-100*sum(b_rs)/b
  }
options(width = 60)
print(prem_res)

u = 20
v = 10 #sample a
w = 10 #sample b
v_yes = 7
v_no = 3
w_yes = 4
w_no = 6
u_yes = v_yes + w_yes
u_no = v_yes + w_yes
v_yes_pc = 100*v_yes/v
w_yes_pc = 100*w_yes/w
vw_yes_pc = v_yes_pc - w_yes_pc

bag = c(rep(1, u_yes), rep(0, u_no))
p = 100
prem_ress = rep(0, p)
for (i in 1:p) {
  bag1 = sample(bag)
  v_res = bag1[1:a]
  w_res = bag1[(a+1):(a+b)]
  prem_ress[i] = 100*sum(v_res)/v - 100*sum(w_res)/w
}
options(width = 60)
print(prem_ress)

table(prem_ress)
pos_extreme = sum(prem_ress >= vw_yes_pc)
extreme = sum(abs(prem_ress) >= abs(vw_yes_pc))
