import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.axes_grid.inset_locator import inset_axes

plt.rcParams.update({'font.size':8, 'lines.linewidth':1})

fig = plt.figure('axes',figsize=(3.33, 3.33))
ax = fig.add_subplot(111)

methods = ['mics']
colors = ['red']

cmap = cm.get_cmap('gist_heat')
for method, color in zip(methods, colors):
    O = np.load('%s_overlap.npy' % method)
    ax.imshow(1-O, cmap=cmap)
#     ax.grid(color='black', linestyle='-')
#     plt.xticks(np.arange(len(O)-1)+0.5)
#     plt.yticks(np.arange(len(O)-1)+0.5)
    ax.set_xticks([])
    ax.set_yticks([])
    for (i, j), val in np.ndenumerate(O):
        if val >= 0.02:
            ax.annotate("%.2f" % val, xy=(j,i), fontsize=5.5, va='center', ha='center', color=cmap(np.round(val)))

# fig, ax = plt.subplots(2,1,sharex=True)
# fig.subplots_adjust(hspace=0.1)
# fig.set_size_inches(w=3.5,h=6)
# title = ['(a)','(b)']
# ymax = [40000, 1600]
# lw = 1.0
# m = 12
# inset = []
# cmap = cm.get_cmap('gist_gray')
# for k, axis in enumerate(ax):
#     data = np.genfromtxt('histogram_%s.csv' % case[k], delimiter=',', skip_header=1,
#                          names=['indx','u0'] + [str(j) for j in range(m)])
#     for j in range(m):
#         axis.plot(data['u0'], data[str(j)], linewidth=lw)
#     axis.set_xlim([-120,3])
#     axis.set_ylim([0, ymax[k]])
#     axis.set_ylabel('Frequency')

#     L = 1.5
#     inset = inset_axes(axis, width=L, height=L, loc=2)
#     inset.set_xticks([])
#     inset.set_yticks([])
#     O = np.load('overlap_%s.npy' % case[k])
#     inset.grid(color='black', linestyle='-', linewidth=2)
#     inset.imshow(1-O, cmap=cmap)
#     for (i, j), val in np.ndenumerate(O):
#         if val >= 0.03:
#             inset.annotate("%.2f" % val, xy=(j,i), fontsize=2*L,
#                            va='center', ha='center', color=cmap(np.round(val)))

# axis.set_xlabel(r'$u_0$')
plt.savefig('overlap_matrix.pdf', format='pdf', dpi=600, bbox_inches='tight')
plt.show()
# exit()

# # loc = ['upper left','upper left','upper right','upper right']


# llim = [-6263,-6263,-6263,-6263]
# ulim = [-6140,-6140,-6140,-6140]
# #xy = [[(0.85, 0.01),(0.75, 0.14),(0.67, 0.36),(0.47, 0.65)], \
# #      [(0.84, 0.02),(0.76, 0.14),(0.67, 0.30),(0.44, 0.68)], \
# #      [(0.84, 0.92),(0.75, 0.54),(0.67, 0.28),(0.25, 0.20)]]
# #captions = [r'$\Delta t = 1 \mathrm{fs}$',  \
# #            r'$\Delta t = 2 \mathrm{fs}$',  \
# #            r'$\Delta t = 3 \mathrm{fs}$',  \
# #            r'$\Delta t = 4 \mathrm{fs}$']
# xy = [[(0.87, 0.05),(0.72, 0.2),(0.6, 0.2),(0.47, 0.63)], \
#       [(0.87, 0.05),(0.72, 0.2),(0.6, 0.2),(0.47, 0.8)], \
#       [(0.87, 0.075),(0.72, 0.25),(0.6, 0.37),(0.47, 0.8)], \
#       [(0.87, 0.075),(0.72, 0.23),(0.6, 0.23),(0.47, 0.8)]]

# captions = ['3 fs','4 fs','5 fs','6 fs']
# data = np.genfromtxt(file[0], delimiter=',', skip_header=1, \
#                        names=['step','dt1','dt2','dt3','dt4','dt5','dt6'])
# p1 = ax[0].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'black')
# p2 = ax[0].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'red')
# p3 = ax[0].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'green')
# p4 = ax[0].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'blue')
# #  p5 = ax[i].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'orange')
# #  p6 = ax[i].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'pink')

# ax[0].annotate(captions[0], xy=(0, llim[0]), xytext=xy[0][0], \
#                  textcoords='axes fraction', color=p1[0].get_color(),fontsize=8)
# ax[0].annotate(captions[1], xy=(0, llim[0]), xytext=xy[0][1], \
#                  textcoords='axes fraction', color=p2[0].get_color())
# ax[0].annotate(captions[2], xy=(0, llim[0]), xytext=xy[0][2], \
#                  textcoords='axes fraction', color=p3[0].get_color())
# ax[0].annotate(captions[3], xy=(0, llim[0]), xytext=xy[0][3], \
#                  textcoords='axes fraction', color=p4[0].get_color())  
# ax[0].set_ylim([llim[0],ulim[0]])

# captions = ['3 fs','4 fs','5 fs','6 fs']
# data = np.genfromtxt(file[1], delimiter=',', skip_header=1, \
#                        names=['step','dt1','dt2','dt3','dt4','dt5','dt6'])
# p1 = ax[1].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'black')
# p2 = ax[1].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'red')
# p3 = ax[1].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'green')
# p4 = ax[1].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'blue')
# #  p5 = ax[i].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'orange')
# #  p6 = ax[i].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'pink')

# ax[1].annotate(captions[0], xy=(0, llim[1]), xytext=xy[1][0], \
#                  textcoords='axes fraction', color=p1[0].get_color(),fontsize=8)
# ax[1].annotate(captions[1], xy=(0, llim[1]), xytext=xy[1][1], \
#                  textcoords='axes fraction', color=p2[0].get_color())
# ax[1].annotate(captions[2], xy=(0, llim[1]), xytext=xy[1][2], \
#                  textcoords='axes fraction', color=p3[0].get_color())
# ax[1].annotate(captions[3], xy=(0, llim[1]), xytext=xy[1][3], \
#                  textcoords='axes fraction', color=p4[0].get_color())  
# ax[1].set_ylim([llim[1],ulim[1]])



# captions = ['3 fs','4 fs','5 fs','6 fs']
# data = np.genfromtxt(file[3], delimiter=',', skip_header=1, \
#                        names=['step','dt1','dt2','dt3','dt4','dt5','dt6'])
# #p1 = ax[3].plot(data['step']*1e-6, data['dt1'], linewidth=lw, color = 'black')
# #p2 = ax[3].plot(data['step']*1e-6, data['dt2'], linewidth=lw, color = 'red')
# #p3 = ax[3].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'green')
# #p4 = ax[3].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'blue')
# p1 = ax[2].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'black')
# p2 = ax[2].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'red')
# p3 = ax[2].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'green')
# p4 = ax[2].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'blue')

# ax[2].annotate(captions[0], xy=(0, llim[3]), xytext=xy[3][0], \
#                  textcoords='axes fraction', color=p1[0].get_color(),fontsize=8)
# ax[2].annotate(captions[1], xy=(0, llim[3]), xytext=xy[3][1], \
#                 textcoords='axes fraction', color=p2[0].get_color())
# ax[2].annotate(captions[2], xy=(0, llim[3]), xytext=xy[3][2], \
#                  textcoords='axes fraction', color=p3[0].get_color())
# ax[2].annotate(captions[3], xy=(0, llim[3]), xytext=xy[3][3], \
#                  textcoords='axes fraction', color=p4[0].get_color())  
# ax[2].set_ylim([llim[3],ulim[3]])



# captions = ['2 fs','3 fs','4 fs','5 fs']
# data = np.genfromtxt(file[2], delimiter=',', skip_header=1, \
#                        names=['step','dt1','dt2','dt3','dt4','dt5'])#,'dt6'])
# #p1 = ax[2].plot(data['step']*1e-6, data['dt1'], linewidth=lw, color = 'black')
# p1 = ax[3].plot(data['step']*1e-6, data['dt2'], linewidth=lw, color = 'magenta')
# p2 = ax[3].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'black')
# p3 = ax[3].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'red')
# p4 = ax[3].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'green')
# #p6 = ax[i].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'pink')

# ax[3].annotate(captions[0], xy=(0, llim[2]), xytext=xy[2][0], \
#                  textcoords='axes fraction', color=p1[0].get_color(),fontsize=8)
# ax[3].annotate(captions[1], xy=(0, llim[2]), xytext=xy[2][1], \
#                  textcoords='axes fraction', color=p2[0].get_color())
# ax[3].annotate(captions[2], xy=(0, llim[2]), xytext=xy[2][2], \
#                  textcoords='axes fraction', color=p3[0].get_color())
# ax[3].annotate(captions[3], xy=(0, llim[2]), xytext=xy[2][3], \
#                  textcoords='axes fraction', color=p4[0].get_color())  
# ax[3].set_ylim([llim[2],ulim[2]])



# #captions = ['3 fs','4 fs','5 fs','6 fs']
# #data = np.genfromtxt(file[3], delimiter=',', skip_header=1, \
#                   #     names=['step','dt1','dt2','dt3']) #,'dt4'])
# #p1 = ax[3].plot(data['step']*1e-6, data['dt1'], linewidth=lw, color = 'black')
# #p2 = ax[3].plot(data['step']*1e-6, data['dt2'], linewidth=lw, color = 'red')
# #p3 = ax[3].plot(data['step']*1e-6, data['dt3'], linewidth=lw, color = 'green')
# #p4 = ax[3].plot(data['step']*1e-6, data['dt4'], linewidth=lw, color = 'blue')
# #  p5 = ax[i].plot(data['step']*1e-6, data['dt5'], linewidth=lw, color = 'orange')
# #  p6 = ax[i].plot(data['step']*1e-6, data['dt6'], linewidth=lw, color = 'pink')

# #ax[3].annotate(captions[0], xy=(0, llim[3]), xytext=xy[3][0], \
#          #        textcoords='axes fraction', color=p1[0].get_color(),fontsize=8)
# #ax[3].annotate(captions[1], xy=(0, llim[3]), xytext=xy[3][1], \
#           #       textcoords='axes fraction', color=p2[0].get_color())
# #ax[3].annotate(captions[2], xy=(0, llim[3]), xytext=xy[3][2], \
#            #      textcoords='axes fraction', color=p3[0].get_color())
# #ax[3].annotate(captions[3], xy=(0, llim[3]), xytext=xy[3][3], \
#             #     textcoords='axes fraction', color=p4[0].get_color())  
# #ax[3].set_ylim([llim[3],ulim[3]])





# #ax[3].set_aspect(2)
# for i in range(4):
#   ax[i].set_ylabel('Total Energy (kcal/mol)')
# ax[2].set_xlabel('Time (ns)')
# ax[3].set_xlabel('Time (ns)')
# plt.xlim((0,6))
# #frame = ax[2].legend.get_frame()
# ax[0].annotate('(a)', xy=(0,llim[0]), xytext = (0.05,0.9) , textcoords= 'axes fraction', fontsize=8)
# ax[1].annotate('(b)', xy=(0,llim[1]), xytext = (0.05,0.9) , textcoords= 'axes fraction', fontsize=8)
# ax[2].annotate('(c)', xy=(0,llim[2]), xytext = (0.05,0.05), textcoords= 'axes fraction', fontsize=8)
# ax[3].annotate('(d)', xy=(0,llim[3]), xytext = (0.05,0.05), textcoords= 'axes fraction', fontsize=8)

