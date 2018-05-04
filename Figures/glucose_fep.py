import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

plt.rcParams.update({'font.size':8, 'lines.linewidth':1.0})

filebase = 'glucose_fep'
kB = 1.987e-3 # kcal/mol.K
T = 300.0 # K

data = pd.read_csv('%s.csv' % filebase)
# data = data[data.index <= 20]
unsampled = data[data['Sampled'] == 0]
sampled = data[data['Sampled'] == 1]

fig, (errors,ax) = plt.subplots(2, 1, sharex='col', gridspec_kw={'height_ratios':[1.0, 5.0]})
fig.subplots_adjust(hspace=0.1)
fig.set_size_inches(w=3.33, h=2.33)

for axis in ['top','bottom','left','right']:
    ax.spines[axis].set_linewidth(1.0)

methods = ['mbar', 'mics']
colors = ['blue', 'red']
formats = ['s', 'o']

options = {'markersize':3, 'markeredgewidth':0.3, 'linewidth':0.5}
for method, color, fmt in zip(methods, colors, formats):
    f = 'f_%s' % method
    df = 'df_%s' % method

    plt.axes(ax)
    plt.plot(sampled['lambda_vdwl'] + sampled['lambda_coul'], kB*T*sampled[f], fmt,
             color=color, label='%s: sampled state' % method.upper(), **options)
    plt.plot(unsampled['lambda_vdwl'] + unsampled['lambda_coul'], kB*T*unsampled[f], fmt,
             mfc='none', color=color, label='%s: perturbation' % method.upper(), **options)

    plt.axes(errors)
    plt.errorbar(sampled['lambda_vdwl'] + sampled['lambda_coul'], np.zeros(len(sampled)), kB*T*sampled[df],
                 fmt=fmt, color=color, capsize=1, label = '_nolegend_', **options)
    plt.errorbar(unsampled['lambda_vdwl'] + unsampled['lambda_coul'], np.zeros(len(unsampled)), kB*T*unsampled[df],
                 fmt=fmt, mfc='white', color=color, capsize=1, label = '_nolegend_', **options)

plt.axes(errors)
plt.ylabel(r'$\delta \Delta G$')

plt.axes(ax)
plt.yticks([-20, -15, -10, -5, 0, 5])
plt.xlabel(r'$\lambda_{vdW} + \lambda_C$')
plt.ylabel(r'$\Delta G$ (kcal/mol)')
plt.legend(loc='lower left', frameon=False, prop={'size': 7})

print("Saving PDF file...")
plt.savefig('../%s.pdf' % filebase, format='pdf', dpi=600, bbox_inches='tight')
plt.show()
