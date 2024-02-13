export const sortAsc = (arr: number[]) => arr.sort((a, b) => a - b)
export const sum = (arr: number[]) => arr.reduce((a, b) => a + b, 0)
export const mean = (arr: number[]) => sum(arr) / arr.length
export const max = (arr: number[]) => arr.reduce((a, c) => a > c ? a : c)

export const cumSum = (arr: number[]) => {
	const result: number[] = []
	let current = 0
	for (const val of arr) {
		current += val
		result.push(current)
	}
	return result
}

export const sd = (arr: number[]) => {
	const mu = mean(arr)
	const diffArr = arr.map((a) => (a - mu) ** 2)
	return Math.sqrt(sum(diffArr) / (arr.length - 1))
}

export const sortedAscQuantile = (sorted: number[], q: number) => {
	const pos = (sorted.length - 1) * q
	const base = Math.floor(pos)
	const rest = pos - base
	let result = sorted[base]
	if (sorted[base + 1] !== undefined) {
		result += rest * (sorted[base + 1] - sorted[base])
	}
	return result
}

export const quantile = (arr: number[], q: number) => sortedAscQuantile(sortAsc(arr), q)
export const sortedAscMin = (sorted: number[]) => sorted[0]
export const sortedAscMax = (sorted: number[]) => sorted[sorted.length - 1]
export const unique = <T>(arr: T[]) => Array.from(new Set(arr))
export const removeIndex = <T>(arr: T[], index: number) => arr.splice(index, 1)

export const arrLinSearch = <T>(arr: T[], item: T) => {
	let result = -1
	for (let index = 0; index < arr.length; index += 1) {
		const elem = arr[index]
		if (elem === item) {
			result = index
			break
		}
	}
	return result
}

export const generalSort = <T>(x: T, y: T) => (x > y ? 1 : x < y ? -1 : 0)
export const numberSort = (x: number, y: number) => x - y

export const desiredOrderSort = <T>(ord: T[]) => {
	return (a: T, b: T) => {
		let result = 0
		const ai = ord.indexOf(a)
		const bi = ord.indexOf(b)
		if (ai !== -1 || bi !== -1) {
			if (ai === -1) {
				result = 1
			} else if (bi === -1) {
				result = -1
			} else if (ai > bi) {
				result = 1
			} else if (ai < bi) {
				result = -1
			}
		}
		return result
	}
}
type NestedArrIter<T> = {
	arrIndices: number[]
	done: boolean
	nestedArr: T[][]
}

export const beginNestedArrIter = <T>(nestedArr: T[][]): NestedArrIter<T> => {
	const arrIndices = [] as number[]
	for (let arrIndex = 0; arrIndex < nestedArr.length; arrIndex += 1) {
		arrIndices.push(0)
	}
	return {
		arrIndices: arrIndices,
		done: false,
		nestedArr: nestedArr,
	}
}

export const getCurrentNestedArrValues = <T>(iter: NestedArrIter<T>) => {
	const facets = [] as T[]
	for (let facetSetIndex = 0; facetSetIndex < iter.nestedArr.length; facetSetIndex += 1) {
		const setValueIndex = iter.arrIndices[facetSetIndex]
		facets.push(iter.nestedArr[facetSetIndex][setValueIndex])
	}
	return facets
}

export const nextNestedArrIter = <T>(iter: NestedArrIter<T>) => {
	let nestedArrCurrentSetIndex = iter.arrIndices.length - 1
	while (true) {
		if (nestedArrCurrentSetIndex == -1) {
			iter.done = true
			break
		}
		if (iter.arrIndices[nestedArrCurrentSetIndex] >= iter.nestedArr[nestedArrCurrentSetIndex].length - 1) {
			iter.arrIndices[nestedArrCurrentSetIndex] = 0
			nestedArrCurrentSetIndex -= 1
		} else {
			iter.arrIndices[nestedArrCurrentSetIndex] += 1
			break
		}
	}
}

export const expandGrid = <T>(input: T[][]): T[][] => {
	const result: T[][] = []
	for (const nestedArrIter = beginNestedArrIter(input); !nestedArrIter.done; nextNestedArrIter(nestedArrIter)) {
		const nestedArrs = getCurrentNestedArrValues(nestedArrIter)
		result.push(nestedArrs)
	}
	return result
}

export const moveToFront = <T>(arr: T[], item: T) => {
	const result = arr.filter((x) => x !== item)
	result.unshift(item)
	return result
}

export type SummariseSpec<RowType, CountsType> = {
	data: RowType[]
	groups: string[]
	defaultCounts: CountsType | (() => CountsType)
	filter?: (row: RowType) => boolean
	getKey: (row: RowType, key: string) => any
	addRow: (row: RowType, counts: CountsType) => void
}

export const summarise = <RowType, CountsType>({
	data,
	groups,
	defaultCounts,
	filter,
	getKey,
	addRow,
}: SummariseSpec<RowType, CountsType>) => {
	const getDefaultCounts = () =>
		typeof defaultCounts === "function" ? (<() => CountsType>defaultCounts)() : { ...defaultCounts }
	let groupedCounts: any = {}
	if (groups.length === 0) {
		groupedCounts = { total: getDefaultCounts() }
	}

	for (const row of data) {
		if (filter ? filter(row) : true) {
			if (groups.length === 0) {
				addRow(row, groupedCounts.total)
			}

			let currentGroupCount = groupedCounts
			for (let groupIndex = 0; groupIndex < groups.length; groupIndex += 1) {
				const group = groups[groupIndex]
				const key = getKey(row, group)

				if (groupIndex == groups.length - 1) {
					if (currentGroupCount[key] === undefined) {
						currentGroupCount[key] = getDefaultCounts()
					}
					addRow(row, currentGroupCount[key])
				} else {
					if (currentGroupCount[key] === undefined) {
						currentGroupCount[key] = {}
					}
					currentGroupCount = currentGroupCount[key]
				}
			}
		}
	}

	return groupedCounts
}

export const flattenMap = (map: any, existing: any[]) => {
	let result: any[] = []
	const isObject = (val: any) => val && typeof val === "object" && val.constructor === Object
	for (const key of Object.keys(map)) {
		const nested = map[key]
		const newExisting = [...existing]
		newExisting.push(key)
		if (isObject(nested) && isObject(Object.values(nested)[0])) {
			result = result.concat(flattenMap(nested, newExisting))
		} else {
			for (const val of Object.values(nested)) {
				newExisting.push(val)
			}
			result.push(newExisting)
		}
	}
	return result
}

export const arrayToMap = (arr: any[], names: string[]) => {
	const result: any = {}
	for (let index = 0; index < arr.length; index += 1) {
		result[names[index]] = arr[index]
	}
	return result
}

export const aoaToAos = (aoa: any[][], names: string[]) => aoa.map((arr) => arrayToMap(arr, names))

export const summariseAos = <RowType, CountsType extends Record<string, unknown>>(
	spec: SummariseSpec<RowType, CountsType>,
	modOnCompletion?: (aosRow: any) => void
) => {
	const numberCols: string[] = []
	for (const group of spec.groups) {
		for (const dataRow of spec.data) {
			const dataVal = spec.getKey(dataRow, group)
			if (dataVal !== null && dataVal !== undefined) {
				if (typeof dataVal === "number") {
					numberCols.push(group)
				}
				break
			}
		}
	}

	const summaryMap = summarise(spec)
	const summaryAoa = flattenMap(summaryMap, [])

	const namesStart = [...spec.groups]
	if (namesStart.length === 0) {
		namesStart.push("Total")
	}

	const getDefaultCounts = () =>
		typeof spec.defaultCounts === "function" ? (<() => CountsType>spec.defaultCounts)() : { ...spec.defaultCounts }
	let summaryAos = aoaToAos(summaryAoa, namesStart.concat(Object.keys(getDefaultCounts())))

	for (const summaryRow of summaryAos) {
		for (const numberCol of numberCols) {
			summaryRow[numberCol] = parseFloat(summaryRow[numberCol])
		}
		for (const colname of Object.keys(summaryRow)) {
			if (summaryRow[colname] === "undefined") {
				summaryRow[colname] = undefined
			}
		}
	}

	if (modOnCompletion !== undefined) {
		summaryAos = summaryAos.map((row) => {
			modOnCompletion(row)
			return row
		})
	}

	return summaryAos
}

export const seq = (from: number, to: number, by: number) => {
	const result = <number[]>[]
	for (let val = from; val < to; val += by) {
		result.push(val)
	}
	return result
}

export const sample = <T>(arr: T[], nInit: number) => {
	const sample = [...arr]
	const length = sample.length
	const n = Math.max(Math.min(nInit, length), 0)
	const last = length - 1
	for (let index = 0; index < n; index++) {
		const rand = Math.round(Math.random() * (last - index) + index)
		const temp = sample[index]
		sample[index] = sample[rand]
		sample[rand] = temp
	}
	const result = sample.slice(0, n)
	return result
}
