//@ts-ignore
import {VirtualizedList} from "/virtualized-list.js"

//
// SECTION Utilities
//

const addDays = (date: Date, days: number) => {
	let dateCopy = new Date(date)
	dateCopy.setDate(date.getDate() + days)
	return dateCopy
}

const allAInB = <T>(arrA: T[], arrB: T[]) => {
	let result = true
	for (let entryA of arrA) {
		if (!arrB.includes(entryA)) {
			result = false
			break
		}
	}
	return result
}

const isString = (val: any) => (typeof val === "string" || val instanceof String)
const isNumber = (val: any) => (typeof val === "number")

const formatDate = (date: Date) => {
	let result = date.toISOString().slice(0, 10)
	return result
}

const notNU = (val: any) => {
	let result = val !== null && val !== undefined
	return result
}

const isObject = (val: any) => {
	let result = val && typeof val === 'object' && val.constructor === Object
	return result
}

const getScrollbarWidths = () => {
	let outer = document.createElement('div')
	outer.style.visibility = "hidden"
	outer.style.overflowY = "scroll"
	document.body.appendChild(outer)

	let inner = document.createElement('div')
	outer.appendChild(inner)

	let scrollbarWidthV = (outer.offsetWidth - inner.offsetWidth)
	outer.removeChild(inner)

	outer.style.overflowY = "hidden"
	outer.style.overflowX = "scroll"

	outer.appendChild(inner)
	let scrollbarWidthH = outer.offsetHeight - inner.offsetHeight

	outer.parentNode!.removeChild(outer);
	return [scrollbarWidthH, scrollbarWidthV];
}

const measureEl = (el: HTMLElement, availableWidth: number, availableHeight: number) => {
	let container = createEl("div")
	container.style.visibility = "hidden"
	container.style.position = "fixed"
	container.style.top = "0px"
	container.style.left = "0px"
	container.style.backgroundColor = "#aa0000"
	container.style.width = availableWidth + "px"
	container.style.height = availableHeight + "px"
	let internalContainer = addDiv(container)
	internalContainer.style.display = "flex"
	addEl(internalContainer, el)
	document.body.appendChild(container)
	let dim = [el.offsetWidth, el.offsetHeight]
	document.body.removeChild(container)
	return dim
}

const fieldsArePresent = (obj: { [key: string]: any }, colnames: string[]) => {
	let result = true
	let missingColnames = []
	for (let colname of colnames) {
		if (obj[colname] === undefined) {
			result = false
			missingColnames.push(colname)
		}
	}
	if (!result) {
		console.error(`fields ${missingColnames.join(",")} not present in`, obj)
	}
	return result
}

const arrLinSearch = <T>(arr: T[], item: T) => {
	let result = -1
	for (let index = 0; index < arr.length; index += 1) {
		let elem = arr[index]
		if (elem == item) {
			result = index
			break
		}
	}
	return result
}

const arrMax = (arr: number[]) => arr.reduce((a, c) => a > c ? a : c)

const arrZeroed = (count: number) => {
	let arr = []
	for (let index = 0; index < count; index += 1) {
		arr.push(0)
	}
	return arr
}

const arrRemoveIndex = (arr: any[], index: number) => {
	arr.splice(index, 1)
}

const seq = (start: number, step: number, count: number) => {
	let result = [start]
	for (let i = 1; i < count; i += 1) {
		result.push(start + i * step)
	}
	return result
}

const dateSeq = (start: string, step: number, count: number) => {

	let result = []

	let startDate = new Date(start)
	let startDayIndex = startDate.getDate()

	for (let i = 0; i < count; i++) {
		let newDayIndex = startDayIndex + step * i
		let newDate = new Date(start)
		newDate.setDate(newDayIndex)
		result.push(newDate)
	}

	return result
}

type SummariseSpec<RowType, CountsType> = {
	data: RowType[],
	groups: string[],
	defaultCounts: CountsType | (() => CountsType),
	filter?: (row: RowType) => boolean,
	getKey: (row: RowType, key: string) => any,
	addRow: (row: RowType, counts: CountsType) => void,
}

const summarise = <RowType, CountsType>(
	{data, groups, defaultCounts, filter, getKey, addRow}: SummariseSpec<RowType, CountsType>,
) => {
	let getDefaultCounts = () => typeof (defaultCounts) === "function" ? (<() => CountsType>defaultCounts)() : {...defaultCounts}
	let groupedCounts: any = {}
	if (groups.length === 0) {
		groupedCounts = { total: getDefaultCounts() }
	}

	for (let row of data) {
		if (filter ? filter(row) : true) {

			if (groups.length === 0) {
				addRow(row, groupedCounts.total)
			}

			let currentGroupCount = groupedCounts
			for (let groupIndex = 0; groupIndex < groups.length; groupIndex += 1) {
				let group = groups[groupIndex]
				let key = getKey(row, group)

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

const flattenMap = (map: any, existing: any[]) => {
	let result: any[] = []
	for (let key of Object.keys(map)) {
		let nested = map[key]
		let newExisting = [...existing]
		newExisting.push(key)
		if (isObject(nested) && isObject(Object.values(nested)[0])) {
			result = result.concat(flattenMap(nested, newExisting))
		} else {
			for (let val of Object.values(nested)) {
				newExisting.push(val)
			}
			result.push(newExisting)
		}
	}
	return result
}

const arrayToMap = (arr: any[], names: string[]) => {
	let result: any = {}
	for (let index = 0; index < arr.length; index += 1) {
		result[names[index]] = arr[index]
	}
	return result
}

const aoaToAos = (aoa: any[][], names: string[]) => aoa.map((arr) => arrayToMap(arr, names))

const summariseAos = <RowType, CountsType>(
	spec: SummariseSpec<RowType, CountsType>,
	modOnCompletion?: (aosRow: any) => void
) => {
	let summaryMap = summarise(spec)
	let summaryAoa = flattenMap(summaryMap, [])

	let namesStart = [...spec.groups]
	if (namesStart.length === 0) {
		namesStart.push("Total")
	}

	let getDefaultCounts = () => typeof (spec.defaultCounts) === "function" ? (<() => CountsType>spec.defaultCounts)() : {...spec.defaultCounts}
	let summaryAos = aoaToAos(summaryAoa, namesStart.concat(Object.keys(getDefaultCounts())))
	if (modOnCompletion !== undefined) {
		summaryAos = summaryAos.map(row => {modOnCompletion(row); return row})
	}

	return summaryAos
}

const getNameOrDefault = (map: any, name: string) => {
	let result = map[name]
	if (result === undefined) {
		result = map.default
	}
	return result
}

const valueOr = <T>(val: T | undefined, or: T) => val === undefined ? or : val

const getColSpecFromGroups = (groups: string[]) => {
	let colSpec: any = {}
	if (groups.length === 0) {
		colSpec.Total = {}
	}
	for (let group of groups) {
		colSpec[group] = {}
	}
	return colSpec
}

const randUnif = (from: number, to: number) => {
	let rand01 = Math.random()
	let range = (to - from)
	let randRange = rand01 * range
	let result = from + randRange
	return result
}

const randNorm = (mean: number, sd: number) => {
	let u1 = 0
	let u2 = 0
	while (u1 === 0) { u1 = Math.random() }
	while (u2 === 0) { u2 = Math.random() }
	let randNorm01 = Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(2.0 * Math.PI * u2)
	let result = randNorm01 * sd + mean
	return result
}

//
// SECTION DOM
//

const XMLNS = "http://www.w3.org/2000/svg"

const TABLE_ROW_HEIGHT_PX = 30
const SCROLLBAR_WIDTHS = getScrollbarWidths()
const SIDEBAR_WIDTH_PX = 120

const YEARS_ = [2020, 2021, 2022] as const
const YEARS = YEARS_ as unknown as number[]
type YearID = (typeof YEARS_)[number]

const ALL_DATAPAGE_IDS_ = ["pariticpants", "weekly-surveys", "bleeds", "counts", "titres"] as const
const ALL_DATAPAGE_IDS = ALL_DATAPAGE_IDS_ as unknown as string[]
type DataPageID = (typeof ALL_DATAPAGE_IDS_)[number]

const ALL_COUNTS_TABLES_ = ["records", "routine-bleeds", "postinfection-bleeds"] as const
const ALL_COUNTS_TABLES = ALL_COUNTS_TABLES_ as unknown as string[]
type CountTableID = (typeof ALL_COUNTS_TABLES_)[number]

const ALL_RECORD_GROUPS_ = ["site", "recruited", "fluArm2022", "covidArm2021",
	"gender", "age", "aboriginal", "prior2020", "prior2021", "prior2022",
	"vax2020", "vax2021", "vax2022"] as const
const ALL_RECORD_GROUPS = ALL_RECORD_GROUPS_ as unknown as string[]
type RecordGroups = (typeof ALL_RECORD_GROUPS_)[number]

const ALL_BLEEDS_GROUPS_ = ["year", "site", "recruited", "fluArm2022", "covidArm2021",
	"gender", "age", "aboriginal", "prior2020", "prior2021", "prior2022",
	"vax2020", "vax2021", "vax2022"] as const
const ALL_BLEEDS_GROUPS = ALL_BLEEDS_GROUPS_ as unknown as string[]
type BleedsGroups = (typeof ALL_BLEEDS_GROUPS_)[number]

const ALL_POSTINF_BLEEDS_GROUPS_ = ALL_BLEEDS_GROUPS_
const ALL_POSTINF_BLEEDS_GROUPS = ALL_POSTINF_BLEEDS_GROUPS_ as unknown as string[]
type PostinfBleedsGroups = (typeof ALL_POSTINF_BLEEDS_GROUPS_)[number]

const ALL_GMT_GROUPS_ = ["year", "day", "site", "virus", "subtype", "eggcell",
	"gender", "recruited", "age_group", "prior2020", "prior2021", "prior2022"] as const
const ALL_GMT_GROUPS = ALL_GMT_GROUPS_ as unknown as string[]
type GMTGroups = (typeof ALL_GMT_GROUPS_)[number]

const ALL_GMR_GROUPS_ = ["year", "site", "virus", "subtype", "eggcell",
	"gender", "recruited", "age_group", "prior2020", "prior2021", "prior2022"] as const
const ALL_GMR_GROUPS = ALL_GMR_GROUPS_ as unknown as string[]
type GMRGroups = (typeof ALL_GMR_GROUPS_)[number]

const DOWNLOAD_CSV: { [key: string]: string } = {}

const createEl = (name: string) => document.createElement(name)
const createDiv = () => createEl("div")
const createSvg = (name: string) => document.createElementNS(XMLNS, name)
const setSvg = (el: SVGElement, name: string, val: string) => el.setAttributeNS(null, name, val)
const setEl = (el: HTMLElement, name: string, val: string) => el.setAttribute(name, val)

const createTextline = (line: string) => {
	let div = createDiv()
	div.textContent = line
	return div
}

const addEl = <T1 extends HTMLElement|SVGElement, T2 extends HTMLElement|SVGElement>(parent: T1, el: T2) => {
	parent.appendChild(el)
	return el
}

const removeEl = (parent: HTMLElement, el: HTMLElement) => {
	parent.removeChild(el)
	return el
}

const addDiv = (parent: HTMLElement) => addEl(parent, createDiv())
const addTextline = (parent: HTMLElement, line: string) => addEl(parent, createTextline(line))

const removeChildren = (element: HTMLElement) => {
	while (element.lastChild) {
		element.removeChild(element.lastChild)
	}
}

const replaceChildren = (parent: HTMLElement, newChild: HTMLElement) => {
	removeChildren(parent)
	addEl(parent, newChild)
}

const createTableDesc = () => {
	let container = createDiv()
	container.style.overflowX = "scroll"
	container.style.maxWidth = "100%"
	let desc = addDiv(container)
	desc.style.whiteSpace = "nowrap"
	return {container: container, desc: desc}
}

const createSwitch = <SingleOpt extends string | number, OptType extends SingleOpt | SingleOpt[]>(
	init: OptType, opts: SingleOpt[], onUpdate: (opt: OptType) => void,
	forOpt?: (opt: SingleOpt, optElement: HTMLElement, updateSelected: (newSel: OptType) => void) => void,
) => {
	let switchElement = createDiv()
	let currentSel = init
	let multiple = Array.isArray(init)

	const isSelected = (opt: SingleOpt) => {
		let result = (!multiple && opt === currentSel) ||
			(multiple && arrLinSearch(<SingleOpt[]>currentSel, opt) !== -1)
		return result
	}

	for (let opt of opts) {
		let optElement = addDiv(switchElement)
		optElement.style.paddingTop = "5px"
		optElement.style.paddingBottom = "5px"
		optElement.style.cursor = "pointer"
		optElement.style.textAlign = "center"

		let normalCol = "var(--color-background)"
		let hoverCol = "var(--color-background2)"
		let selectedCol = "var(--color-selected)"

		if (isSelected(opt)) {
			optElement.style.backgroundColor = selectedCol
		} else {
			optElement.style.backgroundColor = normalCol
		}

		optElement.addEventListener("mouseover", (event) => {
			if (!isSelected(opt)) {
				optElement.style.backgroundColor = hoverCol
			}
		})
		optElement.addEventListener("mouseout", (event) => {
			if (!isSelected(opt)) {
				optElement.style.backgroundColor = normalCol
			}
		})

		optElement.addEventListener("click", async (event) => {
			if (!multiple && opt !== currentSel) {

				for (let child of switchElement.childNodes) {
					(<HTMLElement>child).style.backgroundColor = normalCol
				}
				optElement.style.backgroundColor = selectedCol
				currentSel = <OptType>opt
				onUpdate(<OptType>opt)

			} else if (multiple) {

				let optIndex = arrLinSearch(<SingleOpt[]>currentSel, opt)
				if (optIndex !== -1) {
					optElement.style.backgroundColor = normalCol
					arrRemoveIndex(<SingleOpt[]>currentSel, optIndex)
				} else {
					optElement.style.backgroundColor = selectedCol;
					(<SingleOpt[]>currentSel).push(opt)
				}
				onUpdate(currentSel)

			}
		})

		optElement.textContent = `${opt}`

		if (forOpt !== undefined) {
			forOpt(opt, optElement, (newSel: OptType) => { currentSel = newSel })
		}
	}

	return switchElement
}

const createTableCell = (widthPx: number) => {
	let cellElement = createEl("td")
	cellElement.style.width = widthPx + "px"
	cellElement.style.textAlign = "center"
	cellElement.style.verticalAlign = "middle"
	cellElement.style.whiteSpace = "nowrap"
	return cellElement
}

const createTableCellString = (widthPx: number, string: string) => {
	let cellElement = createTableCell(widthPx)
	cellElement.textContent = string
	if (string === MISSING_STRING) {
		cellElement.style.color = "var(--color-text-muted)"
	}
	return cellElement
}

const createTableTitle = (title: string, downloadable: boolean) => {
	let titleElement = createDiv()
	titleElement.style.display = "flex"
	titleElement.style.alignItems = "center"
	titleElement.style.justifyContent = "center"
	titleElement.style.backgroundColor = "var(--color-background2)"
	titleElement.style.height = TABLE_ROW_HEIGHT_PX + "px"
	titleElement.style.border = "1px solid var(--color-border)"
	titleElement.style.boxSizing = "border-box"
	titleElement.style.whiteSpace = "nowrap"
	titleElement.textContent = title

	if (downloadable) {
		titleElement.style.cursor = "pointer"
		titleElement.textContent += " ⇓ (download)"

		titleElement.addEventListener("click", (event) => {
			let csv = DOWNLOAD_CSV[title]
			if (csv) {
				let hidden = <HTMLLinkElement>createEl("a")
				hidden.href = "data:text/csv;charset=utf-8," + encodeURI(csv)
				hidden.target = "_blank"
				// @ts-ignore
				hidden.download = title + ".csv"
				hidden.click()
			} else {
				console.error(`table '${title}' does not have a csv to download`)
			}
		})
	}

	return titleElement
}

const applyTableHeaderRowStyle = (node: HTMLElement) => {
	node.style.display = "flex"
	node.style.height = TABLE_ROW_HEIGHT_PX + "px"
	node.style.backgroundColor = "var(--color-background2)"
	//node.style.borderLeft = "1px solid var(--color-border)"
	//node.style.borderRight = "1px solid var(--color-border)"
	node.style.boxSizing = "border-box"
}

const applyCellContainerStyle = (node: HTMLElement, width: number) => {
	node.style.display = "flex"
	node.style.width = width.toFixed(0) + "px"
	node.style.alignItems = "center"
	node.style.justifyContent = "center"
	node.style.whiteSpace = "nowrap"
}

const createTableHeaderRow = <T>(colSpec: {[key: string]: TableColSpecFinal<T>}) => {
	let headerRow = createDiv()
	applyTableHeaderRowStyle(headerRow)

	let rowWidth = 0 //SCROLLBAR_WIDTHS[1]
	for (let colname of Object.keys(colSpec)) {
		let colWidthPx = colSpec[colname].width
		rowWidth += colWidthPx
		let cell = addDiv(headerRow)
		applyCellContainerStyle(cell, colWidthPx)
		cell.textContent = colname
	}

	headerRow.style.width = rowWidth + "px"
	return headerRow
}

const createTableFilterRow = <T>(colSpec: {[key: string]: TableColSpecFinal<T>}, onInput: any) => {
	let filterRow = createDiv()
	applyTableHeaderRowStyle(filterRow)

	let rowWidth = 0 //SCROLLBAR_WIDTHS[1]
	let colnameIndex = 0
	for (let colname of Object.keys(colSpec)) {
		let colWidthPx = colSpec[colname].width
		rowWidth += colWidthPx
		let cellContainer = addDiv(filterRow)
		applyCellContainerStyle(cellContainer, colWidthPx)
		cellContainer.style.position = "relative"

		let questionMarkWidth = 20

		let cell = <HTMLInputElement>addEl(cellContainer, createEl("input"))
		cell.type = "text"
        cell.autocomplete = "off"
        cell.placeholder = "Filter..."
		cell.style.width = (colWidthPx - questionMarkWidth) + "px"
		cell.style.boxSizing = "border-box"
		cell.addEventListener("input", (event) => {
			onInput(colname, (<HTMLTextAreaElement>event.target).value)
		})

		let questionMark = addDiv(cellContainer)
		questionMark.style.padding = "2px"
		questionMark.style.width = questionMarkWidth + "px"
		questionMark.style.textAlign = "center"
		questionMark.style.cursor = "pointer"
		questionMark.textContent = "?"

        let helpText = "Case-sensitive. Supports regular expressions (e.g. ^male). For numbers, you can type >x and <x (e.g. >40)"
        let helpEl = createDiv()
        helpEl.textContent = helpText
        helpEl.style.position = "absolute"
        helpEl.style.top = "100%"
        helpEl.style.backgroundColor = "var(--color-background2)"
        helpEl.style.padding = "10px"
        helpEl.style.width = "200px"
        helpEl.style.border = "1px solid var(--color-border)"
        helpEl.style.zIndex = "999"
        helpEl.style.whiteSpace = "normal"

        if (colnameIndex == 0) {
        	helpEl.style.left = "0px"
        }
		if (colnameIndex == Object.keys(colSpec).length - 1) {
        	helpEl.style.right = "0px"
        }

        let helpVisible = false
        questionMark.addEventListener("click", () => {
        	if (helpVisible) {
        		removeEl(cellContainer, helpEl)
        	} else {
        		addEl(cellContainer, helpEl)
        	}
        	helpVisible = !helpVisible
        })

        colnameIndex += 1
	}

	filterRow.style.width = rowWidth + "px"
	return filterRow
}

const getTableBodyHeight = (tableHeight: number) => {
	let result = tableHeight - TABLE_ROW_HEIGHT_PX * 3
	return result
}

const createTableBodyContainer = (tableHeight: number) => {
	let tableBodyContainer = createDiv()
	tableBodyContainer.style.overflowY = "scroll"
	tableBodyContainer.style.maxHeight = getTableBodyHeight(tableHeight) + "px"
	tableBodyContainer.style.boxSizing = "border-box"
	return tableBodyContainer
}

const createTableBody = () => {
	let tableBody = createDiv()
	return tableBody
}

const getTableRowBackgroundColor = (rowIndex: number) => {
	let result = "var(--color-background)"
	if (rowIndex % 2 == 1) {
		result = "var(--color-background2)"
	}
	return result
}

const createTableDataRow = (rowIndex: number) => {
	let rowElement = createEl("tr")
	rowElement.style.height = TABLE_ROW_HEIGHT_PX + "px"
	rowElement.style.backgroundColor = getTableRowBackgroundColor(rowIndex)
	return rowElement
}

type TableColSpec<RowType> = {
	access?: ((row: RowType) => any) | string,
	format?: (val: any) => string,
	width?: number,
	filter?: (row: RowType, val: any) => boolean,
}

type TableColSpecFinal<RowType> = {
	access: (row: RowType) => any,
	format: (val: any) => string,
	width: number,
	filter: (row: RowType, val: any) => boolean,
	filterVal: string,
}

const MISSING_STRING = "(missing)"

const createTableElementFromAos = <RowType extends { [key: string]: any }>(
	{aos, colSpecInit, defaults, title, forRow, getTableHeightInit, onFilterChange}: {
		aos: RowType[],
		colSpecInit: { [key: string]: TableColSpec<RowType> },
		title: string,
		defaults?: TableColSpec<RowType>,
		forRow?: (row: RowType) => void,
		getTableHeightInit?: () => number,
		onFilterChange?: (filteredData: RowType[]) => void,
	}
) => {

	let getTableHeight = getTableHeightInit ?? (() => window.innerHeight - SCROLLBAR_WIDTHS[0])

	let table = createDiv()
	table.style.maxWidth = "100%"
	let titleElement = addEl(table, createTableTitle(title, true))
	DOWNLOAD_CSV[title] = ""

	let colnames = Object.keys(colSpecInit)
	DOWNLOAD_CSV[title] += colnames.join(",") + "\n"

	// NOTE(sen) Fill in missing spec entries
	let colSpec: { [key: string]: TableColSpecFinal<RowType> } = {}
	for (let colname of colnames) {
		let spec = colSpec[colname]
		let specInit = colSpecInit[colname]

		let accessInit = specInit.access ?? defaults?.access ?? colname
		if (isString(accessInit)) {
			let colname = <string>accessInit
			accessInit = (rowData) => rowData[colname]
		}

		let access = <(row: RowType) => any>accessInit
		let format = (x: any) => {
			let result = MISSING_STRING
			if (x !== undefined && x !== null && x !== "undefined") {
				let formatTest = specInit.format ?? defaults?.format
				if (formatTest !== undefined && formatTest !== null) {
					result = formatTest(x)
				} else {
					result = `${x}`
				}
			}
			return result
		}

		colSpec[colname] = {
			access: access,
			format: format,
			width: specInit.width ?? defaults?.width ?? 100,
			filter: specInit.filter ?? defaults?.filter ?? ((row, val) => {
				let data = access(row)
				let formattedData = format(data)
				let passed = true

				if ((val.startsWith(">") || val.startsWith("<")) && isNumber(data)) {
					let valNumber = parseFloat(val.slice(1))
					if (!isNaN(valNumber)) {
						switch (val[0]) {
						case ">": {passed = data >= valNumber} break;
						case "<": {passed = data <= valNumber} break;
						}
					}
				} else {
					try {
						let re = new RegExp(val)
						let reResult = formattedData.search(re)
						passed = reResult !== -1
					} catch (e) {
						passed = formattedData.includes(val)
					}
				}

				return passed
			}),
			filterVal: "",
		}
	}

	let tableWidthPx = 0
	for (let colname of colnames) {
		tableWidthPx += colSpec[colname].width
	}

	if (aos.length > 0) {

		let hscrollContainer = addDiv(table)
		hscrollContainer.style.overflowX = "scroll"
		hscrollContainer.style.boxSizing = "border-box"
		hscrollContainer.style.borderLeft = "1px solid var(--color-border)"
		hscrollContainer.style.borderRight = "1px solid var(--color-border)"

		let headerRow = addEl(hscrollContainer, createTableHeaderRow(colSpec))
		addEl(hscrollContainer, createTableFilterRow(colSpec, (colname: string, filterVal: any) => {
			colSpec[colname].filterVal = filterVal
			aosFiltered = getAosFiltered()
			virtualizedList.setRowCount(aosFiltered.length)
		}))

		let tableBodyHeight = getTableBodyHeight(getTableHeight())
		let tableBodyContainer = addEl(hscrollContainer, createTableBodyContainer(getTableHeight()))
		tableBodyContainer.style.width = tableWidthPx + "px"

		const getAosFiltered = () => {
			let aosFiltered: RowType[] = []
			for (let rowIndex = 0; rowIndex < aos.length; rowIndex += 1) {
				let rowData = aos[rowIndex]

				let passedColFilters = true
				for (let otherColname of colnames) {
					let spec = colSpec[otherColname]
					passedColFilters = passedColFilters && spec.filter(rowData, spec.filterVal)
				}

				if (passedColFilters) {
					aosFiltered.push(rowData)
				}
			}
			onFilterChange?.(aosFiltered)
			return aosFiltered
		}

		let aosFiltered = getAosFiltered()

		const virtualizedList = new VirtualizedList(tableBodyContainer, {
			height: tableBodyHeight,
			rowCount: aosFiltered.length,
			renderRow: (rowIndex: number) => {
				let rowData = aosFiltered[rowIndex]
				let rowElement = createTableDataRow(rowIndex)

				for (let colname of colnames) {
					let spec = colSpec[colname]
					let colData = spec.access(rowData)
					let colDataFormatted = spec.format(colData)
					let width = spec.width - SCROLLBAR_WIDTHS[1] / colnames.length
					addEl(rowElement, createTableCellString(width, colDataFormatted))
				}

				return rowElement
			},
			rowHeight: TABLE_ROW_HEIGHT_PX,
		});

		window.addEventListener("resize", () => {
			let newTableBodyHeight = getTableBodyHeight(getTableHeight())
			if (newTableBodyHeight != tableBodyHeight) {
				tableBodyHeight = newTableBodyHeight
				tableBodyContainer.style.maxHeight = newTableBodyHeight + "px"
				virtualizedList.resize(newTableBodyHeight)
			}
		})

		const resetScroll = () => {
			try {
				virtualizedList.scrollToIndex(1, 0)
				virtualizedList.scrollToIndex(0, 0)
			} catch (e) {}
		}

		window.addEventListener("popstate", resetScroll)
		window.addEventListener("pushState", resetScroll)
		window.addEventListener("replaceState", resetScroll)

		for (let rowIndex = 0; rowIndex < aos.length; rowIndex += 1) {
			let rowData = aos[rowIndex]

			for (let colname of colnames) {
				let spec = colSpec[colname]
				let colData = spec.access(rowData)
				let colDataFormatted = spec.format(colData)
				DOWNLOAD_CSV[title] += "\"" + colDataFormatted + "\","
			}

			DOWNLOAD_CSV[title] += "\n"
			forRow?.(rowData)
		}
	}

	return { table: table, width: tableWidthPx }
}

const initPassword = (state: State) => {
	let container = createDiv()
	let label = addDiv(container)
	let input = <HTMLInputElement>addEl(container, createEl("input"))
	let button = addDiv(container)
	let buttonText = addDiv(button)
	let errorText = addDiv(container)

	input.type = "password"
	input.style.width = "75vw"
	input.style.height = "2em"

	label.textContent = "Password"
	buttonText.textContent = "Submit"

	container.style.width = "75vw"
	container.style.margin = "auto"
	container.style.marginTop = "5vh"

	button.style.cursor = "pointer"
	button.style.border = "1px solid var(--color-border)"
	button.style.height = "50px"
	button.style.width = "50%"
	button.style.margin = "auto"
	button.style.marginTop = "10px"
	button.style.display = "flex"
	button.style.alignItems = "center"
	button.style.justifyContent = "center"

	errorText.style.color = "var(--color-error)"
	errorText.style.textAlign = "center"
	errorText.style.marginTop = "10px"

	button.addEventListener("click", async (e) => {
		if (buttonText.textContent !== "...") {
			errorText.textContent = ""
			if (input.value === "") {
				errorText.textContent = "Password is empty"
			} else {
				buttonText.textContent = "..."
				let fetchResult = await fetchData(input.value)
				if (!fetchResult.success) {
					errorText.textContent = "Not recognized"
				} else {
					localStorage.setItem("password", input.value)
					setGlobalData(state, fetchResult.data)
					switchToCurrentDataPage(state)
				}
				buttonText.textContent = "Submit"
			}
		}
	})

	return container
}

const initLoading = () => {
	let loading = createDiv()
	loading.style.fontSize = "40px"
	loading.style.display = "flex"
	loading.style.alignItems = "center"
	loading.style.justifyContent = "center"
	loading.style.height = "100vh"
	loading.style.margin = "auto"
	loading.textContent = "Loading..."
	return loading
}

const initSidebar = (state: State, widthPx: number, initDataPage: DataPageID) => {
	let sidebar = createDiv()
	sidebar.style.width = widthPx + "px"
	sidebar.style.height = "100vh"
	sidebar.style.flexShrink = "0"
	sidebar.style.display = "flex"
	sidebar.style.flexDirection = "column"
	sidebar.style.justifyContent = "space-between"
	sidebar.style.overflowX = "hidden"
	sidebar.style.overflowY = "scroll"

	let top = addDiv(sidebar)

	let linksContainer = addEl(top, createSwitch(
		initDataPage,
		ALL_DATAPAGE_IDS,
		(dataPage) => {
			let dest: string = dataPage

			switch (dataPage) {
				case "counts": {
					dest = getCountsPageURL(state.settings.counts)
				} break
				case "bleeds": {
					dest = dataPage + "?year=" + state.settings.bleeds.year
				} break
				case "weekly-surveys": {
					dest = dataPage + "?year=" + state.settings.weeklySurveys.year
				} break
				case "titres": {
					dest = getTitresPageURL(state.settings.titres)
				}
			}

			window.history.pushState(null, "", dest)
			switchDataPage(state, dataPage)
		},
		(dataPageOpt, optEl, updateSelected) => {
			window.addEventListener("popstate", () => {
				let dataPage = getDataPageFromURL()
				if (dataPage !== dataPageOpt) {
					optEl.style.background = "var(--color-background)"
				} else {
					optEl.style.background = "var(--color-selected)"
				}
				updateSelected(dataPage)
			})
		}
	))

	let pageSpecific = addDiv(top)
	pageSpecific.style.marginTop = "20px"

	let bottom = addDiv(sidebar)
	let logout = addDiv(bottom)
	logout.textContent = "Logout"
	logout.style.cursor = "pointer"
	logout.addEventListener("mouseover", (event) => logout.style.backgroundColor = "var(--color-selected)")
	logout.addEventListener("mouseleave", (event) => logout.style.backgroundColor = "inherit")
	logout.addEventListener("click", (event) => {
		localStorage.removeItem("password")
		switchToPassword(state)
	})

	return { sidebar: sidebar, pageSpecific: pageSpecific }
}

const initDataContainer = (sidebarWidthPx: number) => {
	let container = createDiv()
	container.style.display = "flex"
	return container
}

const initSurveys = (sidebarWidthPx: number) => {
	let hscrollContainer = createDiv()
	hscrollContainer.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	hscrollContainer.style.overflowX = "scroll"
	hscrollContainer.style.overflowY = "hidden"

	let container = addDiv(hscrollContainer)
	container.style.display = "flex"
	container.style.height = `calc(100vh - ${SCROLLBAR_WIDTHS[0]}px)`

	const getDates = (weekCount: number, start: string, end: string, send: string) => {
		let result: any[] = []
		let startDate = new Date(start)
		let endDate = new Date(end)
		let sendDate = new Date(send)
		for (let index = 0; index < weekCount; index += 1) {
			let row = {
				week: `${index + 1}`,
				start: formatDate(addDays(startDate, index * 7)),
				end: formatDate(addDays(endDate, index * 7)),
				send: formatDate(addDays(sendDate, index * 7)),
			}
			result.push(row)
		}
		return result
	}

	let surveyDates2020 = createTableElementFromAos({
		aos: getDates(32, "2020-04-06", "2020-04-12", "2020-04-13"),
		colSpecInit: {week: {}, start: {}, end: {}, send: {}},
		title: "Weekly survey dates 2020",
	})

	let surveyDates2021 = createTableElementFromAos({
		aos: getDates(32, "2021-01-04", "2021-01-10", "2021-01-11"),
		colSpecInit: {week: {}, start: {}, end: {}, send: {}},
		title: "Weekly survey dates 2021",
	})

	let surveyDates2022 = createTableElementFromAos({
		aos: getDates(32, "2022-01-03", "2022-01-09", "2022-01-10"),
		colSpecInit: {week: {}, start: {}, end: {}, send: {}},
		title: "Weekly survey dates 2022",
	})

	let datesContainer = addDiv(container)

	let surveys = addDiv(container)
	let completions = addDiv(container)
	return {
		hscrollContainer: hscrollContainer,
		container: container, surveys: surveys, completions: completions,
		datesContainer: datesContainer,
		datesTables: { 2020: surveyDates2020.table, 2021: surveyDates2021.table, 2022: surveyDates2022.table }
	}
}

const initBleeds = (sidebarWidthPx: number) => {
	let bleeds = createDiv()
	bleeds.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	//bleeds.style.overflowX = "scroll"
	bleeds.style.display = "flex"
	let table = addDiv(bleeds)
	table.style.maxWidth = `calc(100vw - ${sidebarWidthPx}px)`
	return { bleeds: bleeds, table: table }
}

const initParticipants = (sidebarWidthPx: number) => {
	let container = createDiv()
	container.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	let table = addDiv(container)
	table.style.maxWidth = `calc(100vw - ${sidebarWidthPx}px)`
	return { container: container, table: table }
}

const TITRES_HELP_HEIGHT = 100

const initTitres = (sidebarWidthPx: number) => {
	let container2 = createDiv()
	let help = addDiv(container2)
	help.style.height = TITRES_HELP_HEIGHT + "px"
	help.style.overflow = "scroll"
	addTextline(help, "GMT, GMR tables and the titre plot only use the data displayed in the Titres table (so you can filter the titres table and change everything else on the page).")
	addTextline(help, "Boxplots: minimum - quartile 25 - quartile 75 - maximum. Solid midline: median. Dashed midline: mean (vertical dashed line - 95% CI for mean). Right side: histogram. Numbers: titre measurement counts.")

	let container = addDiv(container2)
	container.style.height = `calc(100vh - ${TITRES_HELP_HEIGHT}px)`
	container.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	container.style.overflow = "hidden"
	container.style.display = "flex"
	container.style.flexDirection = "column"

	let top = addDiv(container)
	top.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	top.style.maxHeight = `calc(100vh / 2 - ${TITRES_HELP_HEIGHT / 2}px)`
	top.style.flex = "1 0"
	top.style.display = "flex"
	top.style.overflow = "hidden"
	let bottom = addEl(container, <HTMLElement>top.cloneNode(true))

	let left = addDiv(top)
	left.style.maxWidth = `calc((100vw - ${SIDEBAR_WIDTH_PX}px) / 2)`
	left.style.flex = "1 0"
	left.style.overflow = "hidden"
	let right = addEl(top, <HTMLElement>left.cloneNode(true))

	let table = addDiv(left)
	table.style.flex = "1 0"
	table.style.display = "flex"

	let plot = addEl(bottom, <HTMLElement>table.cloneNode(true))

	let gmtTable = addEl(right, <HTMLElement>table.cloneNode(true))
	gmtTable.style.height = "50%"

	let gmrTable = addEl(right, <HTMLElement>table.cloneNode(true))

	return { container: container2, table: table, gmtTable: gmtTable, gmrTable: gmrTable, plot: plot }
}

const initCounts = (sidebarWidthPx: number) => {
	let counts = createDiv()
	counts.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	counts.style.overflowX = "hidden"
	counts.style.display = "flex"
	let table = addDiv(counts)
	table.style.maxWidth = `calc(100vw - ${sidebarWidthPx}px)`
	return { counts: counts, table: table }
}

const initCountsSettings = (state: State, init: CountsSettings) => {
	let container = createDiv()

	let tableSwitch = addEl(container, createSwitch(
		init.table,
		<CountTableID[]>ALL_COUNTS_TABLES,
		(table) => {
			state.settings.counts.table = table
			window.history.pushState(null, "", getCountsPageURL(state.settings.counts))
			updateCountsTable(state)
		},
		(table, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getCountSettingsFromURL(state.settings.counts)
				if (settings.table === table) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.table)
			})
		},
	))

	let groupSwitchContainer = addDiv(container)

	let recordsSwitch = createSwitch(
		init.groupsRecords,
		<RecordGroups[]>ALL_RECORD_GROUPS,
		(groups) => {
			state.settings.counts.groupsRecords = groups
			window.history.pushState(null, "", getCountsPageURL(state.settings.counts))
			updateCountsTable(state)
		},
		(group, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getCountSettingsFromURL(state.settings.counts)
				if (settings.groupsRecords.includes(group)) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.groupsRecords)
			})
		},
	)
	recordsSwitch.style.marginTop = "20px"

	let bleedsSwitch = createSwitch(
		init.groupsBleeds,
		<BleedsGroups[]>ALL_BLEEDS_GROUPS,
		(groups) => {
			state.settings.counts.groupsBleeds = groups
			window.history.pushState(null, "", getCountsPageURL(state.settings.counts))
			updateCountsTable(state)
		},
		(group, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getCountSettingsFromURL(state.settings.counts)
				if (settings.groupsBleeds.includes(group)) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.groupsBleeds)
			})
		},
	)
	bleedsSwitch.style.marginTop = "20px"

	let postinfBleedsSwitch = createSwitch(
		init.groupsPostinfBleeds,
		<PostinfBleedsGroups[]>ALL_POSTINF_BLEEDS_GROUPS,
		(groups) => {
			state.settings.counts.groupsPostinfBleeds = groups
			window.history.pushState(null, "", getCountsPageURL(state.settings.counts))
			updateCountsTable(state)
		},
		(group, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getCountSettingsFromURL(state.settings.counts)
				if (settings.groupsBleeds.includes(group)) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.groupsPostinfBleeds)
			})
		},
	)
	postinfBleedsSwitch.style.marginTop = "20px"

	return {
		container: container,
		groupSwitchContainer: groupSwitchContainer,
		recordsSwitch: recordsSwitch,
		bleedsSwitch: bleedsSwitch,
		postinfBleedsSwitch: postinfBleedsSwitch,
	}
}

const initTitresSettings = (state: State, init: TitresSettings) => {
	let container = createDiv()

	let gmtGroupsSwitchLabel = addDiv(container)
	gmtGroupsSwitchLabel.textContent = "GMT groups"

	let gmtGroupsSwitch = createSwitch(
		init.groupsGMTs,
		<GMTGroups[]>ALL_GMT_GROUPS,
		(groups) => {
			state.settings.titres.groupsGMTs = groups
			window.history.pushState(null, "", getTitresPageURL(state.settings.titres))
			updateGMTs(state)
		},
		(group, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getTitresSettingsFromURL(state.settings.titres)
				if (settings.groupsGMTs.includes(group)) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.groupsGMTs)
			})
		},
	)

	addEl(container, gmtGroupsSwitch)

	let gmrGroupsSwitchLabel = addDiv(container)
	gmrGroupsSwitchLabel.textContent = "GMR groups"
	gmrGroupsSwitchLabel.style.marginTop = "30px"

	let gmrGroupsSwitch = createSwitch(
		init.groupsGMRs,
		<GMRGroups[]>ALL_GMR_GROUPS,
		(groups) => {
			state.settings.titres.groupsGMRs = groups
			window.history.pushState(null, "", getTitresPageURL(state.settings.titres))
			updateGMRs(state)
		},
		(group, el, updateSelected) => {
			window.addEventListener("popstate", () => {
				let settings = getTitresSettingsFromURL(state.settings.titres)
				if (settings.groupsGMRs.includes(group)) {
					el.style.backgroundColor = "var(--color-selected)"
				} else {
					el.style.backgroundColor = "var(--color-background)"
				}
				updateSelected(settings.groupsGMRs)
			})
		},
	)

	addEl(container, gmrGroupsSwitch)

	return container
}

const getParticipantsKey = (row: any, group: string) => {
	let key = row[group]
	switch (group) {
		case "recruited": { key = row.recruitment_year; } break
		case "aboriginal": { key = row.atsi; } break
		case "fluArm2022": { key = row.consent_fluArm2022; } break
		case "covidArm2021": { key = row.consent_covidArm2021; } break
		case "age": { key = row.age_group; } break
		case "vax2020": { key = row.study_year_vac_2020; } break
		case "vax2021": { key = row.study_year_vac_2021; } break
		case "vax2022": { key = row.study_year_vac_2022; } break
	}
	return key
}

const addCountsExplanatoryNotes = (node: HTMLElement, groups: string[]) => {
	if (groups.length > 0) {
		addTextline(node, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}

	for (let group of groups) {
		switch (group) {
			case "recruited": { addTextline(node, "recruited - year the participant was recruited"); } break
			case "prior2020": { addTextline(node, "prior2020 - vaccination count between 2015-2019 inclusive"); } break
			case "prior2021": { addTextline(node, "prior2021 - vaccination count between 2016-2020 inclusive"); } break
			case "prior2022": { addTextline(node, "prior2022 - vaccination count between 2017-2021 inclusive"); } break
			case "vax2020": { addTextline(node, "vax2020 - vaccinated in 2020 (vaccination recorded or day14 bleed taken or day14 titre present)"); } break
			case "vax2021": { addTextline(node, "vax2021 - vaccinated in 2021 (vaccination recorded or day14 bleed taken or day14 titre present)"); } break
			case "vax2022": { addTextline(node, "vax2022 - vaccinated in 2022 (vaccination recorded or day14 bleed taken or day14 titre present)"); } break
			case "fluArm2022": { addTextline(node, "fluArm2022 - flu arm as per 2022 consent"); } break
			case "covidArm2021": { addTextline(node, "covidArm2021 - covid arm as per 2021 consent"); } break
		}
	}
}

const createCountsRecordsTable = (data: Data, groups: string[]) => {

	let withdrawalData = data.withdrawn

	let withdrawals: { [key: string]: boolean } = {}
	for (let row of withdrawalData) {
		if (row.withdrawn === 1 && row.withdrawn_reentered !== 1) {
			withdrawals[row.pid] = true
		}
	}

	let groupedCounts = summarise({
		data: data.participants,
		groups: groups,
		defaultCounts: { total: 0, notWithdrawn: 0, consent2022: 0, bled2020: 0, bled2021: 0, bled2022: 0 },
		filter: (row) => row.pid !== undefined && row.pid.length >= 3,
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			let withdrawn = withdrawals[row.pid] === true
			let notWithdrawn = !withdrawn
			counts.total += 1
			if (notWithdrawn) {
				counts.notWithdrawn += 1
			}

			let consentDate = row.date_fluArm2022
			if (consentDate !== undefined) {
				if (consentDate.startsWith("2022")) {
					counts.consent2022 += 1
				}
			}

			if (row.bled2020 === 1) { counts.bled2020 += 1 }
			if (row.bled2021 === 1) { counts.bled2021 += 1 }
			if (row.bled2022 === 1) { counts.bled2022 += 1 }
		}
	})

	let groupedCountsFlat = flattenMap(groupedCounts, [])

	let colSpec = getColSpecFromGroups(groups)
	colSpec.total = {}
	colSpec.notWithdrawn = {}
	colSpec.consent2022 = {}
	colSpec.bled2020 = {}
	colSpec.bled2021 = {}
	colSpec.bled2022 = {}

	let countsAos = aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	let countsTableDesc = createTableDesc()
	addTextline(countsTableDesc.desc, "total - total records in redcap")
	addTextline(countsTableDesc.desc, "notWithdrawn - total records in redcap who are not withdrawn")
	addTextline(countsTableDesc.desc, "consent2022 - total records in redcap whose latest flu conset date is from 2022")
	addTextline(countsTableDesc.desc, "bled2020 - total records in redcap that have any bleed date in 2020")
	addTextline(countsTableDesc.desc, "bled2021 - total records in redcap that have any bleed date in 2021")
	addTextline(countsTableDesc.desc, "bled2022 - total records in redcap that have any bleed date in 2022")
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	let descDim = measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Record counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableResult.table)

	return countsTableContainer
}

const createCountsBleedsTable = (data: Data, groups: string[]) => {

	let groupedCounts = summarise({
		data: data.bleed_dates,
		groups: groups,
		defaultCounts: { fluDay0: 0, fluDay7: 0, fluDay14: 0, fluDay220: 0, covDay0: 0, covDay7: 0, covDay14: 0 },
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			const isPresent = (val: any) => val !== null && val !== undefined && val !== ""
			if (isPresent(row.flu_day_0)) { counts.fluDay0 += 1 }
			if (isPresent(row.flu_day_7)) { counts.fluDay7 += 1 }
			if (isPresent(row.flu_day_14)) { counts.fluDay14 += 1 }
			if (isPresent(row.flu_day_220)) { counts.fluDay220 += 1 }
			if (isPresent(row.covid_day_0)) { counts.covDay0 += 1 }
			if (isPresent(row.covid_day_7)) { counts.covDay7 += 1 }
			if (isPresent(row.covid_day_14)) { counts.covDay14 += 1 }
		}
	})

	let groupedCountsFlat = flattenMap(groupedCounts, [])

	let colSpec = getColSpecFromGroups(groups)
	colSpec.fluDay0 = {}
	colSpec.fluDay7 = {}
	colSpec.fluDay14 = {}
	colSpec.fluDay220 = {}
	colSpec.covDay0 = {}
	colSpec.covDay7 = {}
	colSpec.covDay14 = {}

	let countsAos = aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	let countsTableDesc = createTableDesc()
	addTextline(countsTableDesc.desc, "Routine bleeds that have a date in redcap")
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	let descDim = measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Routine bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableResult.table)

	return countsTableContainer
}

const createCountsPostinfBleedsTable = (data: Data, groups: string[]) => {

	let groupedCounts = summarise({
		data: data.postinf_bleed_dates,
		groups: groups,
		defaultCounts: { day7: 0, day14: 0, day30: 0 },
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			const isPresent = (val: any) => val !== null && val !== undefined && val !== ""
			if (isPresent(row.day7)) { counts.day7 += 1 }
			if (isPresent(row.day14)) { counts.day14 += 1 }
			if (isPresent(row.day30)) { counts.day30 += 1 }
		}
	})

	let groupedCountsFlat = flattenMap(groupedCounts, [])

	let colSpec = getColSpecFromGroups(groups)
	colSpec.day7 = {}
	colSpec.day14 = {}
	colSpec.day30 = {}

	let countsAos = aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	let countsTableDesc = createTableDesc()
	addTextline(countsTableDesc.desc, "Postnfection bleeds that have a date in redcap")
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	let descDim = measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Postinfection bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableResult.table)

	return countsTableContainer
}

const createBleedsTable = (downloadCsv: { [key: string]: string }, data: any, year: number) => {

	let tableResult = createTableElementFromAos({
		aos: data.bleed_dates.filter((row: any) => {
			let result = row.year === year
			if (result) {
				const notMissing = (val: any) => val !== "" && val !== undefined && val !== null
				result = notMissing(row.flu_day_0) ||
					notMissing(row.flu_day_7) ||
					notMissing(row.flu_day_14) ||
					notMissing(row.flu_day_220)
				if (year > 2020) {
					result = result ||
						notMissing(row.covid_day_0) ||
						notMissing(row.covid_day_7) ||
						notMissing(row.covid_day_14)
				}
			}
			return result
		}),
		colSpecInit: {
			pid: {},
			day0: { access: "flu_day_0" },
			day7: { access: "flu_day_7" },
			day14: { access: "flu_day_14" },
			day220: { access: "flu_day_220" },
			day0Covid: { access: "covid_day_0" },
			day7Covid: { access: "covid_day_7" },
			day14Covid: { access: "covid_day_14" },
		},
		title: "Bleed dates",
	})

	return tableResult.table
}

const createParticipantsTable = (downloadCsv: { [key: string]: string }, data: Data) => {

	let tableResult = createTableElementFromAos({
		aos: data.participants,
		colSpecInit: {
			pid: {},
			site: {},
			email: { width: 450 },
			mobile: { width: 200 },
			gender: {},
			atsi: { width: 50 },
			dob: {},
			date_screening: { width: 150 },
			age_screening: { width: 150, format: (x) => x.toFixed(0) },
			recruitment_year: { width: 150 },
		},
		title: "Participants",
	})

	return tableResult.table
}

const createTitreTable = (data: Data, onFilterChange: (filteredData: any[]) => void) => {
	let tableResult = createTableElementFromAos({
		aos: data.titres,
		colSpecInit: {
			pid: {},
			site: {},
			year: {},
			day: {},
			virus: {width: 250},
			subtype: {},
			eggcell: {access: "virus_egg_cell"},
			titre: {},
			gender: {},
			prior2020: {},
			prior2021: {},
			prior2022: {},
			atsi: { width: 50 },
			dob: {},
			date_screening: { width: 150 },
			age: { access: "age_screening", format: (x) => x.toFixed(0) },
			age_group: {},
			recruitment_year: { width: 150 },
			vax2020: {access: "study_year_vac_2020"},
			vax2021: {access: "study_year_vac_2021"},
			vax2022: {access: "study_year_vac_2022"},
		},
		title: "Titres",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 2 - SCROLLBAR_WIDTHS[0],
		onFilterChange: onFilterChange,
	})

	return tableResult.table
}

const createTitreGMTTable = (titreData: any[], groups: string[]) => {

	let titreSummary = summariseAos({
		data: titreData,
		groups: groups,
		defaultCounts: {titres: 0, logtitreSum: 0},
		getKey: (row, group) => {
			let result = row[group]
			switch (group) {
			case "eggcell": {result = row.virus_egg_cell} break
			case "recruited": {result = row.recruitment_year} break
			}
			return result
		},
		addRow: (row, counts) => {counts.titres += 1, counts.logtitreSum += Math.log(row.titre)}
	}, (row) => {
		row.logmean = row.logtitreSum / row.titres
		row.GMT = Math.exp(row.logmean)
	})

	let colSpec: any = {}
	for (let group of groups) {
		colSpec[group] = {}
		if (group === "virus") {
			colSpec[group].width = 150
		}
	}
	colSpec.titres = {}
	colSpec.GMT = {format: (x: any) => x.toFixed(0)}

	let tableResult = createTableElementFromAos({
		aos: titreSummary,
		colSpecInit: colSpec,
		title: "GMT",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - SCROLLBAR_WIDTHS[0],
	})

	return tableResult.table
}

const createTitreGMRTable = (titreData: any[], groups: string[]) => {
	let table = createDiv()

	let ratios = summariseAos({
		data: titreData,
		groups: ["pid"].concat(ALL_GMR_GROUPS),
		defaultCounts: {d0: null, d14: null},
		getKey: (row, group) => {
			let result = row[group]
			switch (group) {
			case "eggcell": {result = row.virus_egg_cell} break
			case "recruited": {result = row.recruitment_year} break
			}
			return result
		},
		addRow: (row, counts) => {
			switch (row.day) {
			case 0: counts.d0 = row.titre; break;
			case 14: counts.d14 = row.titre; break;
			}
		}
	})

	let ratioSummary = summariseAos({
		data: ratios,
		groups: groups,
		defaultCounts: {ratios: 0, logratioSum: 0},
		getKey: (row, group) => row[group],
		filter: (row) => row.d14 !== null && row.d0 !== null,
		addRow: (row, counts) => {
			counts.ratios += 1, counts.logratioSum += Math.log(row.d14 / row.d0)
		}
	}, (row) => {
		row.logmean = row.logratioSum / row.ratios
		row.GMR = Math.exp(row.logmean)
	})

	let colSpec: any = {}
	for (let group of groups) {
		colSpec[group] = {}
		if (group === "virus") {
			colSpec[group].width = 150
		}
	}
	colSpec.ratios = {}
	colSpec.GMR = {format: (x: any) => x.toFixed(2)}

	let tableResult = createTableElementFromAos({
		aos: ratioSummary,
		colSpecInit: colSpec,
		title: "GMR",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - SCROLLBAR_WIDTHS[0],
	})

	return tableResult.table
}

type Plot<X, Y> = {
	canvas: HTMLCanvasElement,
	renderer: CanvasRenderingContext2D,
	spec: PlotSpec<X, Y>,
	scaleXToPx: (x: X) => number,
	scaleYToPx: (y: Y) => number,
}

type Rect = {
	l: number,
	r: number,
	t: number,
	b: number,
}

const scale = (value: number, valueMin: number, valueMax: number, scaleMin: number, scaleMax: number) => {
	let result = scaleMin
	let scaleRange = scaleMax - scaleMin
	if (scaleRange !== 0) {
		result = scaleRange / 2 + scaleMin
		let valueRange = valueMax - valueMin
		if (valueRange !== 0) {
			let value0 = value - valueMin
			let valueNorm = value0 / valueRange
			let valueScale0 = valueNorm * scaleRange
			result = valueScale0 + scaleMin
		}
	}
	return result
}

const drawRect = (renderer: CanvasRenderingContext2D, rect: Rect, color: string) => {
	renderer.fillStyle = color
	renderer.fillRect(rect.l, rect.t, rect.r - rect.l, rect.b - rect.t)
}

const drawLine = (
	renderer: CanvasRenderingContext2D,
	x1: number, y1: number, x2: number, y2: number,
	color: string, thiccness: number, dashSegments: number[]
) => {
	renderer.strokeStyle = color
	renderer.beginPath()
	renderer.moveTo(x1, y1)
	renderer.lineTo(x2, y2)
	let oldLineWidth = renderer.lineWidth
	renderer.lineWidth = thiccness

	renderer.setLineDash(dashSegments)

	renderer.stroke()

	renderer.lineWidth = oldLineWidth
	renderer.setLineDash([])
}

const getLineShift = (x1: number, y1: number, x2: number, y2: number, thiccness: number) => {
	let lineVec = {x: x2 - x1, y: y2 - y1}
	let linePerpVec = {x: lineVec.y, y: lineVec.x}
	let dx = linePerpVec.x / (linePerpVec.x + linePerpVec.y) * thiccness
	let dy = linePerpVec.y / (linePerpVec.x + linePerpVec.y) * thiccness
	return {dx: dx, dy: dy}
}

const drawDoubleLine = (
	renderer: CanvasRenderingContext2D,
	x1: number, y1: number, x2: number, y2: number,
	color: string, color2: string, thiccness: number, dashSegments: number[],
	flipShade?: boolean
) => {
	let {dx, dy} = getLineShift(x1, y1, x2, y2, thiccness)
	if (flipShade) {
		dx = -dx
		dy = -dy
	}

	drawLine(renderer, x1, y1, x2, y2, color, thiccness, dashSegments)
	drawLine(renderer, x1 + dx, y1 + dy, x2 + dx, y2 + dy, color2, thiccness, dashSegments)
}


const rectShrink = (rect: Rect, amount: number) => {
	return {l: rect.l + amount, r: rect.r - amount, t: rect.t + amount, b: rect.b - amount}
}

const drawRectOutline = (renderer: CanvasRenderingContext2D, rect: Rect, color: string, thiccness: number) => {
	let halfThicc = thiccness / 2
	drawLine(renderer, rect.l - halfThicc, rect.t, rect.r + halfThicc, rect.t, color, thiccness, [])
	drawLine(renderer, rect.r, rect.t, rect.r, rect.b, color, thiccness, [])
	drawLine(renderer, rect.l - halfThicc, rect.b, rect.r + halfThicc, rect.b, color, thiccness, [])
	drawLine(renderer, rect.l, rect.t, rect.l, rect.b, color, thiccness, [])
}

const toRadians = (val: number) => val / 360 * 2 * Math.PI

const drawText = (
	renderer: CanvasRenderingContext2D, text: string, xCoord: number, yCoord: number,
	color: string, angle: number, baseline: CanvasTextBaseline, textAlign: CanvasTextAlign,
	outlineColor?: string
) => {
	renderer.fillStyle = color

	renderer.textBaseline = baseline
	renderer.textAlign = textAlign
	renderer.translate(xCoord, yCoord)
	renderer.rotate(toRadians(angle))

	renderer.font = "16px sans-serif"
	if (outlineColor !== undefined) {
		renderer.miterLimit = 2
		renderer.lineJoin = "round"
		renderer.lineWidth = 3
		renderer.strokeStyle = outlineColor
		renderer.strokeText(text, 0, 0)
	}
	renderer.fillText(text, 0, 0)

	renderer.setTransform(1, 0, 0, 1, 0, 0)
}

const drawPath = (
	renderer: CanvasRenderingContext2D,
	yCoords: (number | null)[], xCoords: number[], color: string
) => {
	renderer.strokeStyle = color
	renderer.beginPath()
	let started = false;
	for (let pointIndex = 0; pointIndex < yCoords.length; pointIndex += 1) {
		let xCoord = xCoords[pointIndex];
		let yCoord = yCoords[pointIndex];
		if (yCoord !== null) {
			if (!started) {
				renderer.moveTo(xCoord, yCoord)
				started = true
			} else {
				renderer.lineTo(xCoord, yCoord)
			}
		}
	}
	renderer.stroke()
}

type BoxplotStats = {
	min: number,
	max: number,
	q25: number,
	median: number,
	q75: number,
	iqr: number,
	mean: number,
	meanSe: number,
}

const arrSum = (arr: number[]) => {
	let sum = 0
	for (let val of arr) {
		sum += val
	}
	return sum
}

const arrMean = (arr: number[]) => {
	let sum = arrSum(arr)
	let mean = sum / arr.length
	return mean
}

const arrSd = (arr: number[]) => {
  let mu = arrMean(arr)
  let diffArr = arr.map((a) => (a - mu) ** 2)
  let sd = Math.sqrt(arrSum(diffArr) / (arr.length - 1))
  return sd
}

const arrSortedAscQuantile = (sorted: number[], q: number) => {
  const pos = (sorted.length - 1) * q
  const base = Math.floor(pos)
  const rest = pos - base
  let result = sorted[base]
  if (sorted[base + 1] !== undefined) {
    result += rest * (sorted[base + 1] - sorted[base])
  }
  return result
}

const getSortedStats = (arr: number[]) => {
	let result: BoxplotStats | null = null
	if (arr.length > 0) {
		let arrSorted = arr.sort((x1, x2) => x1 - x2)
		let q25 = arrSortedAscQuantile(arrSorted, 0.25)
		let q75 = arrSortedAscQuantile(arrSorted, 0.75)
		result = {
			min: arrSorted[0],
			max: arrSorted[arrSorted.length - 1],
			median: arrSortedAscQuantile(arrSorted, 0.5),
			q25: q25,
			q75: q75,
			iqr: q75 - q25,
			mean: arrMean(arrSorted),
			meanSe: arrSd(arrSorted) / Math.sqrt(arr.length),
		}
	}
	return result
}

const addBoxplot = <X, Y>(
	plot: Plot<X, Y>,
	data: any[],
	xNames: string[],
	getX: (row: any) => X,
	getY: (row: any) => Y,
	totalBoxWidth: number,
	color: string,
	altColor: string,
	meanColor: string,
	lineThiccness: number,
) => {

	let summary = summariseAos({
		data: data,
		groups: xNames,
		defaultCounts: () => ({yVals: [] as number[]}),
		getKey: (row, group) => row[group],
		addRow: (row, summ) => {summ.yVals.push(plot.scaleYToPx(getY(row)))}
	}, (summ) => {summ.stats = getSortedStats(summ.yVals)})

	totalBoxWidth = Math.max(totalBoxWidth, 0)
	let boxWidth = totalBoxWidth / 2
	let meanChonkiness = boxWidth
	let medianChonkiness = boxWidth / 2

	for (let boxplotData of summary) {

		let xVal = getX(boxplotData)
		let xCoord = plot.scaleXToPx(xVal)
		let boxLeft = xCoord - boxWidth
		let boxRight = xCoord

		let boxplotBody = {l: boxLeft, b: boxplotData.stats.q75, r: boxRight, t: boxplotData.stats.q25}
		drawRectOutline(plot.renderer, boxplotBody, color, lineThiccness)
		drawRectOutline(plot.renderer, rectShrink(boxplotBody, lineThiccness), altColor, lineThiccness)

		drawDoubleLine(
			plot.renderer,
			boxLeft - medianChonkiness,
			boxplotData.stats.median,
			boxRight,
			boxplotData.stats.median,
			color,
			altColor,
			lineThiccness,
			[]
		)

		drawDoubleLine(
			plot.renderer,
			boxLeft - meanChonkiness,
			boxplotData.stats.mean,
			boxRight,
			boxplotData.stats.mean,
			meanColor,
			altColor,
			lineThiccness,
			[3, 3]
		)

		drawDoubleLine(
			plot.renderer,
			boxLeft - meanChonkiness / 2,
			boxplotData.stats.mean + boxplotData.stats.meanSe * 1.96,
			boxLeft - meanChonkiness / 2,
			boxplotData.stats.mean - boxplotData.stats.meanSe * 1.96,
			meanColor,
			altColor,
			lineThiccness,
			[3, 3]
		)

		drawDoubleLine(
			plot.renderer,
			xCoord,
			boxplotData.stats.q75,
			xCoord,
			boxplotData.stats.max,
			color,
			altColor,
			lineThiccness,
			[],
			true,
		)

		drawDoubleLine(
			plot.renderer,
			xCoord,
			boxplotData.stats.min,
			xCoord,
			boxplotData.stats.q25,
			color,
			altColor,
			lineThiccness,
			[],
			true,
		)
	}
}

type PlotSpec<X, Y> = {
	width: number,
	height: number,
	scaleXData?: (x: X) => number,
	scaleYData?: (y: Y) => number,
	padAxis: Rect,
	padData: Rect,
	xMin: X,
	xMax: X,
	yMin: Y,
	yMax: Y,
	xTicks: X[],
	yTicks: Y[],
}

const beginPlot = <X, Y>(spec: PlotSpec<X, Y>) => {

	let canvas = <HTMLCanvasElement>createEl("canvas")
	canvas.width = spec.width
	canvas.height = spec.height

	let renderer = canvas.getContext("2d")!

	let scaleXData = spec.scaleXData ?? ((x) => x as unknown as number)
	let scaleYData = spec.scaleYData ?? ((y) => y as unknown as number)

	let scaleXToPx = (val: X) => scale(
		scaleXData(val), scaleXData(spec.xMin), scaleXData(spec.xMax),
		spec.padAxis.l + spec.padData.l, spec.width - spec.padAxis.r - spec.padData.r
	)

	let scaleYToPx = (val: Y) => {
		let result = scale(
			scaleYData(val), scaleYData(spec.yMin), scaleYData(spec.yMax),
			spec.height - spec.padAxis.b - spec.padData.b, spec.padAxis.t + spec.padData.t
		)
		return result
	}

	let axisThiccness = 1
	let axisCol = "#bfbdb6"

	// NOTE(sen) Axis lines

	drawRect(
		renderer,
		{l: spec.padAxis.l, r: spec.padAxis.l + axisThiccness,
			t: spec.padAxis.t, b: spec.height - spec.padAxis.b},
		axisCol,
	)

	drawRect(
		renderer,
		{l: spec.padAxis.l, r: spec.width - spec.padAxis.r,
			t: spec.height - spec.padAxis.b - axisThiccness, b: spec.height - spec.padAxis.b},
		axisCol,
	)

	// NOTE(sen) Ticks and grid

	let tickLength = 5
	let tickToText = 5
	let axisTextCol = axisCol

	for (let xTick of spec.xTicks) {
		let xCoord = scaleXToPx(xTick)
		drawRect(
			renderer,
			{l: xCoord, r: xCoord + axisThiccness,
				t: spec.height - spec.padAxis.b, b: spec.height - spec.padAxis.b + tickLength},
			axisCol
		)
		drawText(
			renderer,
			`${xTick}`,
			xCoord,
			spec.height - spec.padAxis.b + tickLength + tickToText,
			axisTextCol,
			0,
			"hanging",
			"center",
		)
	}

	for (let yTick of spec.yTicks) {
		let yCoord = scaleYToPx(yTick)
		drawRect(
			renderer,
			{l: spec.padAxis.l - tickLength, r: spec.padAxis.l,
				t: yCoord - axisThiccness, b: yCoord},
			axisCol
		)
		drawText(
			renderer,
			`${yTick}`,
			spec.padAxis.l - tickLength - tickToText,
			yCoord,
			axisTextCol,
			0,
			"middle",
			"end",
		)
	}

	let result: Plot<X, Y> = {canvas: canvas, renderer: renderer, spec: spec,
		scaleXToPx: scaleXToPx, scaleYToPx: scaleYToPx}
	return result
}

const createTitrePlot = (data: any[]) => {
	let container = createDiv()
	container.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	container.style.maxHeight = `calc(100vh / 2 - ${TITRES_HELP_HEIGHT / 2}px)`
	container.style.overflow = "hidden"

	let allDays = [0, 7, 14, 220]
	let allTitres = [5, 10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240]

	let plot = beginPlot({
		width: window.innerWidth - SIDEBAR_WIDTH_PX,
		height: window.innerHeight / 2 - TITRES_HELP_HEIGHT / 2,
		padAxis: {l: 70, r: 10, t: 10, b: 50},
		padData: {l: 50, r: 50, t: 10, b: 10},
		xMin: -3.5,
		xMax: 223.5,
		yMin: 5,
		yMax: 10240,
		scaleYData: Math.log,
		scaleXData: (x) => x >= 220 ? x - 170 : x,
		yTicks: allTitres,
		xTicks: allDays,
	})

	let lineGroups = summariseAos({
		data: data,
		groups: ALL_GMT_GROUPS.concat("pid").filter(x => x !== "day"),
		defaultCounts: {day0: null, day7: null, day14: null, day220: null},
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			switch (row.day) {
			case 0: {counts.day0 = row.titre} break;
			case 7: {counts.day7 = row.titre} break;
			case 14: {counts.day14 = row.titre} break;
			case 220: {counts.day220 = row.titre} break;
			}
		}
	})

	let lineThreshold = 100
	let lineAlpha = "00"
	if (lineGroups.length <= lineThreshold) {
		let lineAlphaMin = 10
		lineAlpha = Math.round((Math.exp(-0.02 * lineGroups.length) * (255 - lineAlphaMin) + lineAlphaMin)).toString(16).padStart(2, "0")
	}
	let lineColBase = "#61de2a"
	let lineCol = lineColBase + lineAlpha

	let pointAlphaMin = 10
	let pointAlpha = Math.round((Math.exp(-0.02 * lineGroups.length) * (255 - pointAlphaMin) + pointAlphaMin)).toString(16).padStart(2, "0")

	let dayTitreCounts = allDays.map(day => arrZeroed(allTitres.length))

	for (let lineGroup of lineGroups) {
		let yJitter = randUnif(-10, 10)
		let xJitter = randUnif(-10, 10)

		let titres = [lineGroup.day0, lineGroup.day7, lineGroup.day14, lineGroup.day220]
		let yCoords = titres.map((x) => x !== null ? plot.scaleYToPx(x) + yJitter : null)
		let xCoords = allDays.map((x) => plot.scaleXToPx(x) + xJitter)

		if (lineAlpha !== "00") {
			drawPath(plot.renderer, yCoords, xCoords, lineCol)
		}

		for (let titreIndex = 0; titreIndex < titres.length; titreIndex += 1) {
			let yCoord = yCoords[titreIndex]
			let xCoord = xCoords[titreIndex]
			let pointSize = 5
			let pointHalfSize = pointSize / 2
			let pointCol = lineColBase + pointAlpha
			if (yCoord !== null) {
				drawRect(
					plot.renderer,
					{l: xCoord - pointHalfSize, r: xCoord + pointHalfSize,
						t: yCoord - pointHalfSize, b: yCoord + pointHalfSize},
					pointCol
				)
				dayTitreCounts[titreIndex][allTitres.indexOf(titres[titreIndex])] += 1
			}
		}
	}

	let dayTitresCounts01 = dayTitreCounts.map(arr => {
		let max = arrMax(arr)
		return arr.map(val => val / max)
	})

	let dayPxStep = plot.scaleXToPx(allDays[1]) - plot.scaleXToPx(allDays[0])

	let boxLineThiccness = 2
	let boxplotCol = "#ffa600"
	let distColor = "#de61a8"
	let altColor = "#000000"

	let gapX = 10
	let boxWidth = dayPxStep / 2 - gapX
	let distWidth = boxWidth

	let boxplotMeanCol = boxplotCol

	if (lineGroups.length > 1) {
		addBoxplot(
			plot, data, ["day"], (row) => parseInt(row.day), (row) => row.titre,
			boxWidth, boxplotCol, altColor, boxplotMeanCol, boxLineThiccness
		)
	}

	let titrePxStep = plot.scaleYToPx(allTitres[0]) - plot.scaleYToPx(allTitres[1])
	for (let dayIndex = 0; dayIndex < allDays.length; dayIndex += 1) {
		let dayCounts01 = dayTitresCounts01[dayIndex]
		let dayCounts = dayTitreCounts[dayIndex]
		let day = allDays[dayIndex]
		let xCoord = plot.scaleXToPx(day)
		let prevBarRight = null
		for (let count01Index = 0; count01Index < allTitres.length; count01Index += 1) {
			let count01 = dayCounts01[count01Index]
			let count = dayCounts[count01Index]
			let titre = allTitres[count01Index]
			let yCoord = plot.scaleYToPx(titre)

			let barRight = xCoord + boxLineThiccness + distWidth * count01

			let down = titrePxStep / 2
			let up = down
			if (count01Index == 0) {
				down = down / 2
			} else if (count01Index == allTitres.length - 1) {
				up = up / 2
			}

			drawDoubleLine(
				plot.renderer, barRight, yCoord - up, barRight, yCoord + down,
				distColor, altColor, boxLineThiccness, [], true,
			)

			if (prevBarRight !== null) {
				let halfThicc = boxLineThiccness / 2
				let vLeft = prevBarRight
				let vRight = barRight
				if (vLeft > vRight) {
					let temp = vLeft
					vLeft = vRight
					vRight = temp
				}
				drawLine(
					plot.renderer, vLeft - halfThicc, yCoord + down, vRight + halfThicc, yCoord + down,
					distColor, boxLineThiccness, [],
				)
			}

			let countTextCol = "#bfbdb6"
			let lineCountsPad = 5
			drawText(
				plot.renderer, `${count}`, barRight - boxLineThiccness, yCoord, countTextCol, 0, "middle", "end",
				altColor
			)

			prevBarRight = barRight
		}
	}

	addEl(container, plot.canvas as HTMLElement)
	return container
}

const createSurveyTable = (completions: { [key: string]: number[] }, data: any, year: number) => {
	let tableContainer = createDiv()

	let tableResult = createTableElementFromAos({
		aos: data.weekly_surveys.filter((row: any) => row.year === year && row.complete !== 0),
		colSpecInit: { pid: {}, site: {}, week: { access: "survey_index" }, date: {}, ari: {} },
		title: "Completed weekly surveys",
		forRow: (row) => {
			if (completions[row.pid] === undefined) {
				completions[row.pid] = []
			}
			completions[row.pid].push(row.survey_index)
		}
	})

	addEl(tableContainer, tableResult.table)
	return tableContainer
}

const createCompletionsTable = (completions: { [key: string]: number[] }) => {

	let collapsed = []
	for (let [pid, completedSurveys] of Object.entries(completions)) {
		let collapsedCompletions = ""
		if (completedSurveys.length > 0) {
			collapsedCompletions += completedSurveys[0]
			if (completedSurveys.length > 1) {

				let prev = completedSurveys[0]
				let currentStreak = 1
				for (let completeIndex = 1; completeIndex < completedSurveys.length; completeIndex += 1) {
					let compl = completedSurveys[completeIndex]

					if (completeIndex == completedSurveys.length - 1) {

						if (currentStreak > 1) {
							collapsedCompletions += "-"
						} else {
							collapsedCompletions += ", "
						}
						collapsedCompletions += compl

					} else if (compl - prev == currentStreak) {

						currentStreak += 1

					} else {

						if (currentStreak > 1) {
							let lastNotShown = prev + currentStreak - 1
							collapsedCompletions += "-" + lastNotShown
						}

						collapsedCompletions += ", "
						collapsedCompletions += compl
						currentStreak = 1
						prev = compl
					}

				}
			}
		}
		collapsed.push({ pid: pid, completions: collapsedCompletions })
	}

	let tableResult = createTableElementFromAos({
		aos: collapsed,
		colSpecInit: { pid: {}, completions: { width: 300 } },
		title: "Collapsed completions",
	})

	let tableContainer = createDiv()
	addEl(tableContainer, tableResult.table)

	return tableContainer
}

const fetchData = async (password: string) => {
	let success = true
	let data = {}

	if (
		password === "" ||
		password === null ||
		password === undefined ||
		!isString(password)
	) {

		success = false

	} else {

		let address =
			"https://reports2.hcwflustudy.com/api2/" +
			password +
			"/data.json"

		//address = `/pull-redcap/redcap-all_data.json`

		try {
			let resp = await fetch(address)
			data = await resp.json()
		} catch (e) {
			success = false
			console.error(e)
		}
	}

	return { success: success, data: data }
}

const setGlobalData = (state: State, data: any) => {
	state.data = data
	updateCountsTable(state)
	updateBleedsTable(state)
	updateSurveyTables(state)
	updateParticipantsTable(state)
	updateTitres(state)
}

const getDataPageFromURL = () => {
	let path = window.location.pathname.slice(1)
	let result: DataPageID = "counts"
	if (ALL_DATAPAGE_IDS.includes(path)) {
		result = <DataPageID>path
	} else {
		window.history.replaceState(null, "", result)
	}
	return result
}

type CountsSettings = {
	table: CountTableID,
	groupsRecords: RecordGroups[],
	groupsBleeds: BleedsGroups[],
	groupsPostinfBleeds: PostinfBleedsGroups[],
}

type TitresSettings = {
	groupsGMTs: GMTGroups[],
	groupsGMRs: GMRGroups[],
}

const getCountsPageURL = (settings: CountsSettings) => {
	let recordGroups = `record_groups=${settings.groupsRecords.join(",")}`
	let bleedsGroups = `bleeds_groups=${settings.groupsBleeds.join(",")}`
	let postinfBleedsGroups = `postinf_bleeds_groups=${settings.groupsPostinfBleeds.join(",")}`
	let result = `counts?table=${settings.table}&${recordGroups}&${bleedsGroups}&${postinfBleedsGroups}`
	return result
}

const getTitresPageURL = (settings: TitresSettings) => {
	let groupsGMTs = `groupsGMTs=${settings.groupsGMTs.join(",")}`
	let groupsGMRs = `groupsGMRs=${settings.groupsGMRs.join(",")}`
	let result = `titres?${groupsGMTs}&${groupsGMRs}`
	return result
}

const getURLArrayParam = <T>(params: any, paramName: string, allowed: string[], def: T[]) => {
	let urlArr = def
	let needToFixAddress = false

	if (params.has(paramName)) {

		let parArr = params.getAll(paramName)
		let first = parArr[0].split(",")
		if (parArr[0] === "") {
			urlArr = []
		} else if (allAInB(first, allowed)) {
			urlArr = <T[]>first
		} else {
			needToFixAddress = true
		}

		if (parArr.length > 1) {
			needToFixAddress = true
		}

	} else {
		needToFixAddress = true
	}

	return {urlArr: urlArr, needToFixAddress: needToFixAddress}
}

const getURLSingleParam = <T>(params: any, paramName: string, allowed: any[], def: T) => {
	let urlParam = def
	let needToFixAddress = false

	if (params.has(paramName)) {

		let allParams = params.getAll(paramName)
		let toCheck: any = allParams[0]
		if (isNumber(allowed[0])) {
			toCheck = parseInt(allParams[0])
		}
		let paramIsValid = allowed.includes(toCheck)
		if (paramIsValid) {
			urlParam = <T>toCheck
		}

		if (allParams.length > 1 || !paramIsValid) {
			needToFixAddress = true
		}

	} else {
		needToFixAddress = true
	}

	return {urlParam: urlParam, needToFixAddress: needToFixAddress}
}

const getCountSettingsFromURL = (def: CountsSettings) => {
	let urlTable = def.table
	let urlGroupsRecords = def.groupsRecords
	let urlGroupsBleeds = def.groupsBleeds
	let urlGroupsPostinfBleeds = def.groupsPostinfBleeds

	if (window.location.pathname === "/counts") {
		let params = new URLSearchParams(window.location.search)

		let tableRes = getURLSingleParam(params, "table", ALL_COUNTS_TABLES, urlTable)
		urlTable = tableRes.urlParam

		let recordGroupsRes = getURLArrayParam(params, "record_groups", ALL_RECORD_GROUPS, urlGroupsRecords)
		urlGroupsRecords = recordGroupsRes.urlArr

		let bleedGroupsRes = getURLArrayParam(params, "bleeds_groups", ALL_BLEEDS_GROUPS, urlGroupsBleeds)
		urlGroupsBleeds = bleedGroupsRes.urlArr

		let postinfBleedGroupsRes = getURLArrayParam(params, "postinf_bleeds_groups", ALL_POSTINF_BLEEDS_GROUPS, urlGroupsPostinfBleeds)
		urlGroupsPostinfBleeds = postinfBleedGroupsRes.urlArr

		let needToFixAddress =
			tableRes.needToFixAddress ||
			recordGroupsRes.needToFixAddress ||
			bleedGroupsRes.needToFixAddress ||
			postinfBleedGroupsRes.needToFixAddress

		if (needToFixAddress) {
			window.history.replaceState(null, "", getCountsPageURL({
				table: urlTable,
				groupsRecords: urlGroupsRecords,
				groupsBleeds: urlGroupsBleeds,
				groupsPostinfBleeds: urlGroupsPostinfBleeds
			}))
		}
	}

	let result: CountsSettings = {
		table: urlTable,
		groupsRecords: urlGroupsRecords,
		groupsBleeds: urlGroupsBleeds,
		groupsPostinfBleeds: urlGroupsPostinfBleeds,
	}

	return result
}

const getTitresSettingsFromURL = (def: TitresSettings) => {
	let urlGroupsGMTs = def.groupsGMTs
	let urlGroupsGMRs = def.groupsGMRs

	if (window.location.pathname === "/titres") {
		let params = new URLSearchParams(window.location.search)

		let gmtGroupsRes = getURLArrayParam(params, "groupsGMTs", ALL_GMT_GROUPS, urlGroupsGMTs)
		urlGroupsGMTs = gmtGroupsRes.urlArr

		let gmrGroupsRes = getURLArrayParam(params, "groupsGMRs", ALL_GMT_GROUPS, urlGroupsGMRs)
		urlGroupsGMRs = gmrGroupsRes.urlArr

		let needToFixAddress = gmtGroupsRes.needToFixAddress || gmrGroupsRes.needToFixAddress
		if (needToFixAddress) {
			window.history.replaceState(null, "", getTitresPageURL({
				groupsGMTs: urlGroupsGMTs,
				groupsGMRs: urlGroupsGMRs,
			}))
		}
	}

	let result: TitresSettings = {
		groupsGMTs: urlGroupsGMTs,
		groupsGMRs: urlGroupsGMRs,
	}

	return result
}

const getBleedsYearFromURL = (def: YearID) => {
	let urlYear = def

	if (window.location.pathname === "/bleeds") {

		let params = new URLSearchParams(window.location.search)

		let yearRes = getURLSingleParam(params, "year", YEARS, urlYear)
		urlYear = yearRes.urlParam

		if (yearRes.needToFixAddress) {
			window.history.replaceState(null, "", `bleeds?year=${urlYear}`)
		}
	}

	return urlYear
}

const getSurveysYearFromURL = (def: YearID) => {
	let urlYear = def

	if (window.location.pathname === "/weekly-surveys") {

		let params = new URLSearchParams(window.location.search)

		let yearRes = getURLSingleParam(params, "year", YEARS, urlYear)
		urlYear = yearRes.urlParam

		if (yearRes.needToFixAddress) {
			window.history.replaceState(null, "", `weekly-surveys?year=${urlYear}`)
		}
	}

	return urlYear
}

const switchDataPage = (state: State, name: DataPageID) => {
	let oldDataPage = state.currentDataPage
	state.currentDataPage = name
	switch (name) {
		case "weekly-surveys": {
			replaceChildren(state.elements.sidebar.pageSpecific, state.elements.weeklySurveySettings)
			replaceChildren(state.elements.dataContainer, state.elements.weeklySurveys.hscrollContainer)
		} break
		case "bleeds": {
			replaceChildren(state.elements.sidebar.pageSpecific, state.elements.bleedsSettings)
			replaceChildren(state.elements.dataContainer, state.elements.bleeds.bleeds)
		} break
		case "counts": {
			replaceChildren(state.elements.sidebar.pageSpecific, state.elements.countsSettings.container)
			replaceChildren(state.elements.dataContainer, state.elements.counts.counts)
		} break
		case "pariticpants": {
			removeChildren(state.elements.sidebar.pageSpecific)
			replaceChildren(state.elements.dataContainer, state.elements.participants.container)
		} break
		case "titres": {
			replaceChildren(state.elements.sidebar.pageSpecific, state.elements.titresSettings)
			replaceChildren(state.elements.dataContainer, state.elements.titres.container)
		} break;
		default: {
			console.error("data page", name, "does not exist")
			state.currentDataPage = oldDataPage
		}
	}
}

const switchToPassword = (state: State) => replaceChildren(state.domMain, state.elements.password)
const switchToLoading = (state: State) => replaceChildren(state.domMain, state.elements.loading)
const switchToCurrentDataPage = (state: State) => {
	removeChildren(state.domMain)
	addEl(state.domMain, state.elements.sidebar.sidebar)
	addEl(state.domMain, state.elements.dataContainer)
	switchDataPage(state, state.currentDataPage)
}

const updateCountsTable = (state: State) => {
	let tableEl = createDiv()
	let switchEl = createDiv()

	switch (state.settings.counts.table) {
		case "records": {
			tableEl = createCountsRecordsTable(state.data, state.settings.counts.groupsRecords)
			switchEl = state.elements.countsSettings.recordsSwitch
		} break;

		case "routine-bleeds": {
			tableEl = createCountsBleedsTable(state.data, state.settings.counts.groupsBleeds)
			switchEl = state.elements.countsSettings.bleedsSwitch
		} break;

		case "postinfection-bleeds": {
			tableEl = createCountsPostinfBleedsTable(state.data, state.settings.counts.groupsPostinfBleeds)
			switchEl = state.elements.countsSettings.postinfBleedsSwitch
		} break;

		default: console.error("unexpected counts table name", state.settings.counts.table)
	}

	replaceChildren(state.elements.counts.table, tableEl)
	replaceChildren(state.elements.countsSettings.groupSwitchContainer, switchEl)
}

const updateBleedsTable = (state: State) => replaceChildren(
	state.elements.bleeds.table,
	createBleedsTable(DOWNLOAD_CSV, state.data, state.settings.bleeds.year)
)

const updateSurveyTables = (state: State) => {
	replaceChildren(
		state.elements.weeklySurveys.datesContainer,
		state.elements.weeklySurveys.datesTables[state.settings.weeklySurveys.year]
	)
	let completions = {}
	replaceChildren(
		state.elements.weeklySurveys.surveys,
		createSurveyTable(completions, state.data, state.settings.weeklySurveys.year)
	)
	replaceChildren(
		state.elements.weeklySurveys.completions,
		createCompletionsTable(completions)
	)
}

const updateParticipantsTable = (state: State) => replaceChildren(
	state.elements.participants.table,
	createParticipantsTable(DOWNLOAD_CSV, state.data)
)

const updateGMTs = (state: State) => replaceChildren(
	state.elements.titres.gmtTable,
	createTitreGMTTable(state.filteredTitreData, state.settings.titres.groupsGMTs)
)

const updateGMRs = (state: State) => replaceChildren(
	state.elements.titres.gmrTable,
	createTitreGMRTable(state.filteredTitreData, state.settings.titres.groupsGMRs)
)

const updateTitrePlot = (state: State) => replaceChildren(
	state.elements.titres.plot,
	createTitrePlot(state.filteredTitreData)
)

const updateTitres = (state: State) => {
	let titreTable = createTitreTable(state.data, (filteredData) => {
		state.filteredTitreData = filteredData
		updateGMTs(state)
		updateGMRs(state)
		updateTitrePlot(state)
	})
	replaceChildren(state.elements.titres.table, titreTable)
}

type State = {
	data: any,
	filteredTitreData: any[],
	domMain: HTMLElement,
	currentDataPage: DataPageID,
	elements: {
		loading: HTMLElement,
		password: HTMLElement,
		sidebar: {
			sidebar: HTMLElement,
			pageSpecific: HTMLElement,
		},
		weeklySurveySettings: HTMLElement,
		bleedsSettings: HTMLElement,
		countsSettings: {
			container: HTMLElement,
			groupSwitchContainer: HTMLElement,
			recordsSwitch: HTMLElement,
			bleedsSwitch: HTMLElement,
			postinfBleedsSwitch: HTMLElement,
		},
		titresSettings: HTMLElement,
		dataContainer: HTMLElement,
		weeklySurveys: {
			hscrollContainer: HTMLElement,
			container: HTMLElement,
			surveys: HTMLElement,
			completions: HTMLElement,
			datesContainer: HTMLElement,
			datesTables: {
				2020: HTMLElement
				2021: HTMLElement
				2022: HTMLElement
			},
		},
		bleeds: {
			bleeds: HTMLElement,
			table: HTMLElement,
		},
		counts: {
			counts: HTMLElement,
			table: HTMLElement,
		},
		participants: {
			container: HTMLElement,
			table: HTMLElement,
		},
		titres: {
			container: HTMLElement,
			table: HTMLElement,
			gmtTable: HTMLElement,
			gmrTable: HTMLElement,
			plot: HTMLElement,
		},
	},
	settings: {
		weeklySurveys: { year: YearID },
		bleeds: { year: YearID },
		counts: CountsSettings,
		titres: TitresSettings,
	},
}

const initState = (state: State) => {
	state.data = {}
	state.filteredTitreData = []

	state.domMain = document.getElementById("main")!
	state.currentDataPage = getDataPageFromURL()

	const initYear: YearID = 2022
	const initYearBleeds: YearID = getBleedsYearFromURL(initYear)
	const initYearSurvey: YearID = getSurveysYearFromURL(initYear)
	const initCountsSettingsVal: CountsSettings = getCountSettingsFromURL({
		table: "records", groupsRecords: ["site"], groupsBleeds: ["year"], groupsPostinfBleeds: ["year"],
	})
	const initTitresSettingsVal: TitresSettings = getTitresSettingsFromURL({
		groupsGMTs: ["year", "day"], groupsGMRs: ["year"],
	})

	state.elements = {
		loading: initLoading(),
		password: initPassword(state),

		sidebar: initSidebar(state, SIDEBAR_WIDTH_PX, state.currentDataPage),

		weeklySurveySettings: createSwitch(
			initYearSurvey,
			<YearID[]>YEARS,
			(year) => {
				state.settings.weeklySurveys.year = year
				window.history.pushState(null, "", `weekly-surveys?year=${year}`)
				updateSurveyTables(state)
			},
			(year, el, updateSelected) => {
				window.addEventListener("popstate", () => {
					let urlYear = getSurveysYearFromURL(state.settings.weeklySurveys.year)
					if (urlYear == year) {
						el.style.backgroundColor = "var(--color-selected)"
					} else {
						el.style.backgroundColor = "var(--color-background)"
					}
					updateSelected(urlYear)
				})
			},
		),

		bleedsSettings: createSwitch(
			initYearBleeds, YEARS,
			(year) => {
				state.settings.bleeds.year = year
				window.history.pushState(null, "", `bleeds?year=${year}`)
				updateBleedsTable(state)
			},
			(year, el, updateSelected) => {
				window.addEventListener("popstate", () => {
					let urlYear = getBleedsYearFromURL(state.settings.bleeds.year)
					if (urlYear == year) {
						el.style.backgroundColor = "var(--color-selected)"
					} else {
						el.style.backgroundColor = "var(--color-background)"
					}
					updateSelected(urlYear)
				})
			},
		),

		countsSettings: initCountsSettings(state, initCountsSettingsVal),
		titresSettings: initTitresSettings(state, initTitresSettingsVal),

		dataContainer: initDataContainer(SIDEBAR_WIDTH_PX),
		weeklySurveys: initSurveys(SIDEBAR_WIDTH_PX),
		bleeds: initBleeds(SIDEBAR_WIDTH_PX),
		counts: initCounts(SIDEBAR_WIDTH_PX),
		participants: initParticipants(SIDEBAR_WIDTH_PX),
		titres: initTitres(SIDEBAR_WIDTH_PX)
	}

	state.settings = {
		weeklySurveys: { year: initYearSurvey },
		bleeds: { year: initYearBleeds },
		counts: initCountsSettingsVal,
		titres: initTitresSettingsVal,
	}
}

type Data = {
	participants: any[],
	withdrawn: any[],
	bleed_dates: any[],
	consent: any[]
	weekly_surveys: any[],
	postinf_bleed_dates: any[],
	titres: any[],
}

const main = async () => {

	const state: any = {}
	initState(<State>state)

	window.addEventListener("popstate", (event) => {
		state.currentDataPage = getDataPageFromURL()

		switch (state.currentDataPage) {
			case "counts": {
				state.settings.counts = getCountSettingsFromURL(state.settings.counts)
				updateCountsTable(state)
			} break

			case "bleeds": {
				state.settings.bleeds.year = getBleedsYearFromURL(state.settings.bleeds.year)
				updateBleedsTable(state)
			} break

			case "weekly-surveys": {
				state.settings.weeklySurveys.year = getSurveysYearFromURL(state.settings.weeklySurveys.year)
				updateSurveyTables(state)
			} break

			case "titres": {
				state.settings.titres = getTitresSettingsFromURL(state.settings.titres)
				updateTitres(state)
			} break
		}

		switchToCurrentDataPage(state)
	})

	window.addEventListener("resize", (event) => {
		if (state.currentDataPage == "titres") {
			updateTitrePlot(state)
		}
	})

	// NOTE(sen) Attempt to login from local storage
	{
		let password = localStorage.getItem("password")
		if (password === null) {
			switchToPassword(state)
		} else {
			switchToLoading(state)
			let fetchResult = await fetchData(password)
			if (!fetchResult.success) {
				switchToPassword(state)
			} else {
				setGlobalData(state, fetchResult.data)
				switchToCurrentDataPage(state)
			}
		}
	}
}

const historyChangeStateAndDispatchEvent = function(type: string) {
	//@ts-ignore
    const orig: any = history[type];
    return function() {
    	//@ts-ignore
        const rv = orig.apply(this, arguments);
        const e: any = new Event(type);
        e.arguments = arguments;
        window.dispatchEvent(e);
        return rv;
    };
};

history.pushState = historyChangeStateAndDispatchEvent('pushState')
history.replaceState = historyChangeStateAndDispatchEvent('replaceState')

main()

// NOTE(sen) To make this a "module"
export {}

// TODO(sen) Counts of swab results
// TODO(sen) Date filtering
// TODO(sen) Table sorting
// TODO(sen) Titre plots
