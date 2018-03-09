import numpy as np
import matplotlib.pyplot as plt

data = np.genfromtxt('nelder_mead.csv', delimiter=',', names=True)
plt.rcParams.update({'font.size':8})

fig, ax = plt.subplots(2,1,sharex=True, gridspec_kw = {'height_ratios':[1, 3]})
fig.subplots_adjust(hspace=0.0)
fig.set_size_inches(w=3.5,h=3.5)
fig.subplots_adjust(hspace=0.05)

for axis in ['top','bottom','left','right']:
  ax[0].spines[axis].set_linewidth(0.5)
  ax[1].spines[axis].set_linewidth(0.5)

ax[0].plot(data['df'], linewidth=0.5, color = 'blue')
ax[0].set_ylabel("Uncertainty",fontsize=8)
ax[0].set_ylim([0.023,0.026])
ax[0].yaxis.set_ticks([0.023,0.024,0.025,0.026])

ax[1].set_ylim([0,0.45])
ax[1].yaxis.set_ticks([0.0,0.1,0.2,0.3,0.4])
ax[1].plot(data['p1'], linewidth=0.5, color = 'brown')
ax[1].plot(data['p2'], linewidth=0.5, color = 'purple')
ax[1].plot(data['p3'], linewidth=0.5, color = 'green')
ax[1].plot(data['p4'], linewidth=0.5, color = 'red')
ax[1].plot(data['p5'], linewidth=0.5, color = 'cyan')
ax[1].plot(data['p6'], linewidth=0.5, color = 'magenta')
ax[1].plot(data['p7'], linewidth=0.5, color = 'orange')
ax[1].plot(data['p8'], linewidth=0.5, color = 'blue')
ax[1].plot(data['p9'], linewidth=0.5, color = 'black')
#plt.xticks(fontsize=7)
#plt.yticks(fontsize=7)
ax[1].set_xlabel("Iteration",fontsize=8)
ax[1].set_ylabel("Prior probability",fontsize=8)
ax[1].legend([r'$\pi_1$', \
              r'$\pi_2$', \
              r'$\pi_3$', \
              r'$\pi_4$', \
              r'$\pi_5$', \
              r'$\pi_6$', \
              r'$\pi_7$', \
              r'$\pi_8$', \
              r'$\pi_9$'], loc='upper left', frameon=False,fontsize=8, ncol=3)
plt.savefig('nelder_mead.eps', format='eps', dpi=600, bbox_inches='tight')
plt.show()

