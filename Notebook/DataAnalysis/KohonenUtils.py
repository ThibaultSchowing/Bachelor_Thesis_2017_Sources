"""Utility functions to work with the kohonen package."""

import numpy as np
import kohonen
import math
import matplotlib.pylab as pl
import matplotlib.cm as cm
import matplotlib.patches as mpatches
from matplotlib.colors import Normalize
from sklearn.preprocessing import MinMaxScaler
import random
import seaborn as sns
sns.set_style('ticks')


def _discrete_colors_from_classes(classes):
    unique_classes = np.unique(classes)
    color_dict = {}
    colormap = sns.color_palette('muted', n_colors=len(unique_classes))
    for i, unique_class in enumerate(unique_classes):
        color_dict[unique_class] = colormap[i]
    return color_dict


class KohonenMap:
    """Wrapper for a Kohonen map."""

    def __init__(self, side_rows, side_cols, size_vector,
                 metric_name='euclidean_metric'):
        """Register the map as main object."""
        accepted_metrics = ['euclidean_metric', 'cosine_metric']
        assert metric_name in accepted_metrics, 'Use either cosine or ' \
                                                'euclidean metric.'
        if metric_name == 'euclidean_metric':
            metric = kohonen.euclidean_metric
        elif metric_name == 'cosine_metric':
            metric = kohonen.cosine_metric
        # Initializes the Kohonen map as a rectangular map
        params = kohonen.Parameters(dimension=size_vector,
                                    shape=(side_rows, side_cols),
                                    metric=metric)
        kohonen_map = kohonen.Map(params)
        self._map = kohonen_map
        self._trained = False
        self._distances = None
        self._samples_dict = None

    def train(self, input_data, output_data, n_iter,
              learning_rate, neighborhood_size):
        """Train a Kohonen map by presenting a dataset several times."""
        assert not self._trained, 'This map is already trained!'
        assert input_data.shape[1] == self._map.dimension, 'Wrong number of ' \
                                                           'dimensions!'
        self._map._learning_rate = learning_rate
        self._map._neighborhood_size = neighborhood_size

        scaler = MinMaxScaler()
        normalized = scaler.fit_transform(input_data)

        # Presents the whole dataset n_iter times
        for i in range(0, n_iter):
            # Shuffles the order in which the observations come
            order = np.arange(0, normalized.shape[0], 1)
            random.shuffle(order)
            for j in order:
                self._map.learn(normalized[j])
        self._trained = True
        self._compute_umatrix()
        self._construct_samples_for_neurons(normalized)
        self._input_data = input_data
        self._output_data = output_data

    def _compute_umatrix(self):
        """Compute the U-Matrix distances from a trained Kohonen map."""
        assert self._trained, 'You should train the map first!'
        neuron_distances = np.zeros((2 * self._map.neurons.shape[0] - 1,
                                     2 * self._map.neurons.shape[1] - 1))
        neuron_distances -= 1
        for row in range(0, neuron_distances.shape[0]):
            for col in range(0, neuron_distances.shape[1]):
                neuron = row % 2 == 0 and col % 2 == 0
                other = row % 2 != 0 and col % 2 != 0
                if not neuron and not other:
                    if row % 2 != 0:
                        neuron1_row = (row - 1) / 2
                        neuron2_row = (row + 1) / 2
                        neuron1 = self._map.neurons[neuron1_row, col / 2, :]
                        neuron2 = self._map.neurons[neuron2_row, col / 2, :]
                    else:
                        neuron1_col = (col - 1) / 2
                        neuron2_col = (col + 1) / 2
                        neuron1 = self._map.neurons[row / 2, neuron1_col, :]
                        neuron2 = self._map.neurons[row / 2, neuron2_col, :]
                    dist = self._map._metric(neuron1, neuron2)
                    neuron_distances[row, col] = dist
        self.distances = neuron_distances

    def _construct_samples_for_neurons(self, dataset):
        """Compute winning neurons (1-nn) for the dataset."""
        assert self._trained, 'You should train the map first!'
        samples_dict = dict()
        for i in range(self._map.shape[0]):
            for j in range(self._map.shape[1]):
                key = str(i) + ',' + str(j)
                samples_dict[key] = list()
        for i, row in enumerate(dataset):
            j = self._map.winner(row)
            x, y = self._map.flat_to_coords(j)
            key = str(x) + ',' + str(y)
            samples_dict[key].append(i)
        self.samples_dict = samples_dict

    def plot_umatrix(self, plot_points=True, color_classes=False,
                     plot_empty=False, plot_names=False, dataset_output=None,
                     plot_index=False, figsize=None, fontsize=8):
        """Plot the U-Matrix in greyscale colors."""
        assert self._trained, 'You should train the map first!'

        if color_classes or plot_names:
            if self._output_data is None:
                assert dataset_output is not None, 'You should provide an ' \
                                                   'output to plot!'
                self._output_data = dataset_output
        # Creates a colormap for the U-Matrix
        norm = Normalize(vmin=0, vmax=np.amax(self.distances))
        colors = cm.binary
        colors.set_bad('w', 1.)

        # Creates a dict color for classes
        if color_classes:
            class_colors = _discrete_colors_from_classes(self._output_data)
            # To track if a neuron has multiple classes
            one_multi = False

        if figsize is None:
            figsize = (5, 5)
            shape0, shape1 = self._map.shape
            if shape0 > shape1:
                figsize = figsize[0], figsize[1] * float(shape0) / shape1
            elif shape1 > shape0:
                figsize = figsize[0] * float(shape1) / shape0, figsize[1]

        fig = pl.figure(figsize=figsize)
        ax = fig.add_subplot(111)

        for row in range(0, self.distances.shape[0]):
            for col in range(0, self.distances.shape[1]):
                if self.distances[row, col] != -1:
                    x = col
                    y = row
                    c = colors(norm(self.distances[row, col]))
                    circle = pl.Circle((x, y), 0.5, color=c)
                    ax.add_artist(circle)

        if plot_points:
            valid = self.samples_dict is not None
            assert valid, 'You should either call ' \
                          'construct_samples_for_neurons() or set ' \
                          'plot_points to False.'
            # Prints samples on the U-Matrix
            for neuron, samples in self.samples_dict.iteritems():
                neuronx, neurony = neuron.split(',')
                # In U-Matrix there is an extra neuron between each pair
                neuronx = int(neuronx) * 2
                neurony = int(neurony) * 2

                if not plot_empty and len(samples) == 0:
                    continue
                if plot_names or plot_index:
                    display = ''
                    if plot_names and plot_index:
                        size = fontsize
                        for sample_id in samples:
                            name = self._output_data[sample_id]
                            display += str(sample_id) + str(name) + '\n'
                    else:
                        if plot_names:
                            size = fontsize
                            for sample_id in samples:
                                name = self._output_data[sample_id]
                                display += str(name) + '\n'
                        else:
                            size = fontsize
                            for sample_id in samples:
                                display += str(sample_id) + '\n'
                    pl.annotate(display, (neurony, neuronx + 0.25), size=size)
                if color_classes:
                    # Colors the point by the color of the class it contains
                    # or black if multiclass...
                    count_classes = dict()
                    for class_id in np.unique(self._output_data):
                        count_classes[class_id] = 0
                    for sample_id in samples:
                        count_classes[self._output_data[sample_id]] += 1

                    current_max = 0
                    for class_id in np.unique(self._output_data):
                        if count_classes[class_id] > current_max:
                            current_max = count_classes[class_id]
                            max_class = class_id
                            multi_class = False
                        elif count_classes[class_id] == current_max:
                            multi_class = True
                    if multi_class:
                        one_multi = True
                        point_color = 'k'
                    else:
                        point_color = class_colors[max_class]
                    pl.scatter(neurony, neuronx, marker=(5, 0), c=point_color)
                else:
                    if len(samples) > 0:
                        c = 'r'
                    else:
                        c = 'b'
                    pl.scatter(neurony, neuronx, marker=(5, 0), c=c)

        if color_classes:
            patches = []
            for i, class_id in enumerate(np.unique(self._output_data)):
                patches.append(mpatches.Patch(color=class_colors[class_id],
                                              label=str(class_id)))
            if one_multi:
                patches.append(mpatches.Patch(color='k', label='Multi'))
            pl.legend(handles=patches, bbox_to_anchor=(0., 1.02, 1., .102),
                      loc=5, ncol=2, borderaxespad=0.)

        pl.xlim(-0.5, col + 1)
        pl.ylim(row + 0.5, -0.5)
        pl.axis('off')
        pl.show()


class Timeseries(object):
    """Represents some sort of value that changes over time."""

    def __init__(self):
        """Set up this timeseries."""
        super(Timeseries, self).__init__()
        self.ticks = 0

    def __call__(self):
        """Call this timeseries."""
        t = self.ticks
        self.ticks += 1
        return t


class ExponentialTimeseries(Timeseries):
    """Represents an exponential decay process."""

    def __init__(self, initial=1, final=0.2, n_iter=10):
        """Create a new exponential timeseries object."""
        super(ExponentialTimeseries, self).__init__()
        assert initial > 0, "Initial value has to be greater than zero!"
        assert final > 0, "Final value has to be greater than zero!"
        self.initial = math.log(initial)
        self.final = math.log(final)
        self.n_iter = n_iter
        timeserie = np.linspace(self.initial, self.final, self.n_iter)
        self.values = np.exp(timeserie)

    def __call__(self):
        """Return an exponentially-decreasing series of values."""
        if self.ticks < self.n_iter:
            super(ExponentialTimeseries, self).__call__()
        return self.values[self.ticks - 1]

    def plot(self):
        """Plot this timeseries."""
        pl.plot(self.values)
