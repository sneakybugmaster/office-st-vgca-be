package com.vz.backend.util;

public class Quicksort {

	public static <T> void sort(T[] array, Comparator<T> comparator) {

		if (array == null || array.length == 0) {
			return;
		}

		quickSort(array, comparator, 0, array.length - 1);
	}

	private static <T> void quickSort(T[] array, Comparator<T> comparator, int lowerIndex, int higherIndex) {
		int i = lowerIndex;
		int j = higherIndex;

		// calculate pivot number, I am taking pivot as middle index number
		T pivot = array[lowerIndex + (higherIndex - lowerIndex) / 2];
		// Divide into two arrays
		while (i <= j) {
			/**
			 * In each iteration, we will identify a number from left side which is greater
			 * then the pivot value, and also we will identify a number from right side
			 * which is less then the pivot value. Once the search is done, then we exchange
			 * both numbers.
			 */
			while (comparator.compare(array[i], pivot) < 0) {
				i++;
			}
			while (comparator.compare(array[j], pivot) > 0) {
				j--;
			}

			if (i <= j) {
				exchangeNumbers(array, i, j);
				// move index to next position on both sides
				i++;
				j--;
			}
		}
		// call quickSort() method recursively
		if (lowerIndex < j) {
			quickSort(array, comparator, lowerIndex, j);
		}
		if (i < higherIndex) {
			quickSort(array, comparator, i, higherIndex);
		}
	}

	private static <T> void exchangeNumbers(T[] array, int i, int j) {
		T temp = array[i];
		array[i] = array[j];
		array[j] = temp;
	}

	public interface Comparator<T> {

		int compare(T t1, T t2);

	}

}
