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
		if (elem === item) {
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
	const numberCols: string[] = []
	for (let group of spec.groups) {
		for (let dataRow of spec.data) {
			const dataVal = spec.getKey(dataRow, group)
			if (dataVal !== null && dataVal !== undefined) {
				if (isNumber(dataVal)) {
					numberCols.push(group)
				}
				break
			}
		}
	}

	let summaryMap = summarise(spec)
	let summaryAoa = flattenMap(summaryMap, [])

	let namesStart = [...spec.groups]
	if (namesStart.length === 0) {
		namesStart.push("Total")
	}

	let getDefaultCounts = () => typeof (spec.defaultCounts) === "function" ? (<() => CountsType>spec.defaultCounts)() : {...spec.defaultCounts}
	let summaryAos = aoaToAos(summaryAoa, namesStart.concat(Object.keys(getDefaultCounts())))

	for (let summaryRow of summaryAos) {
		for (let numberCol of numberCols) {
			summaryRow[numberCol] = parseFloat(summaryRow[numberCol])
		}
		for (let colname of Object.keys(summaryRow)) {
			if (summaryRow[colname] === "undefined") {
				summaryRow[colname] = undefined
			}
		}
	}

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

const ALL_DATAPAGE_IDS_ = ["participants", "weekly-surveys", "bleeds", "counts", "titres", "problems"] as const
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

const ALL_TITRE_FACETS_ = ALL_GMT_GROUPS_
const ALL_TITRE_FACETS = ALL_TITRE_FACETS_ as unknown as string[]
type TitreFacets = (typeof ALL_TITRE_FACETS_)[number]

const ALL_SUBTYPES_ = ["H1", "H3", "BVic", "BYam"] as const
const ALL_SUBTYPES = ALL_SUBTYPES_ as unknown as string[]
type Subtypes = (typeof ALL_SUBTYPES_)[number]

const ALL_EGGCELL_ = ["egg", "cell"] as const
const ALL_EGGCELL = ALL_EGGCELL_ as unknown as string[]
type EggCell = (typeof ALL_EGGCELL_)[number]

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
	filterValProcess?: (val: string) => string,
}

type TableColSpecFinal<RowType> = {
	access: (row: RowType) => any,
	format: (val: any) => string,
	width: number,
	filter: (row: RowType, val: string) => boolean,
	filterVal: string,
	filterValProcess: (val: string) => string,
}

const MISSING_STRING = "(missing)"

let globalResizeListeners: any[] = []

const clearPageListners = () => {
	for (let listner of globalResizeListeners) {
		window.removeEventListener("resize", listner)
	}
	globalResizeListeners = []
}

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
			filterValProcess: specInit.filterValProcess ?? defaults?.filterValProcess ?? ((x) => x),
		}
	}

	let tableWidthPx = 0
	for (let colname of colnames) {
		tableWidthPx += colSpec[colname].width
	}

	let regenBody = () => {}

	if (aos.length > 0) {

		let hscrollContainer = addDiv(table)
		hscrollContainer.style.overflowX = "scroll"
		hscrollContainer.style.boxSizing = "border-box"
		hscrollContainer.style.borderLeft = "1px solid var(--color-border)"
		hscrollContainer.style.borderRight = "1px solid var(--color-border)"

		let headerRow = addEl(hscrollContainer, createTableHeaderRow(colSpec))
		addEl(hscrollContainer, createTableFilterRow(colSpec, (colname: string, filterVal: any) => {
			colSpec[colname].filterVal = colSpec[colname].filterValProcess(filterVal)
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
			estimatedRowHeight: TABLE_ROW_HEIGHT_PX,
			rowHeight: TABLE_ROW_HEIGHT_PX,
		});

		regenBody = () => {
			let newTableBodyHeight = getTableBodyHeight(getTableHeight())
			if (newTableBodyHeight != tableBodyHeight) {
				tableBodyHeight = newTableBodyHeight
				tableBodyContainer.style.maxHeight = newTableBodyHeight + "px"
				virtualizedList.resize(newTableBodyHeight)
			}
		}

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

	window.addEventListener("resize", regenBody)
	globalResizeListeners.push(regenBody)

	return table
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
		datesTables: { 2020: surveyDates2020, 2021: surveyDates2021, 2022: surveyDates2022 }
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

const addGroupsExplanatoryNotes = (node: HTMLElement, groups: string[]) => {
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

const addCountsExplanatoryNotes = (node: HTMLElement, groups: string[]) => {
	if (groups.length > 0) {
		addTextline(node, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}
	addGroupsExplanatoryNotes(node, groups)
}

const createCountsRecordsTable = (data: Data, groups: RecordGroups[]) => {

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

	let tableEl = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Record counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createCountsBleedsTable = (data: Data, groups: BleedsGroups[]) => {

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

	let tableEl = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Routine bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createCountsPostinfBleedsTable = (data: Data, groups: PostinfBleedsGroups[]) => {

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

	let tableEl = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Postinfection bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc.container)
	addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createBleedsTable = (downloadCsv: { [key: string]: string }, data: any, year: number) => {

	let tableEl = createTableElementFromAos({
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

	return tableEl
}

const createTitreTable = (data: Data, onFilterChange: (filteredData: any[]) => void) => {
	let tableEl = createTableElementFromAos({
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

	return tableEl
}

const getTitreKey = (row: any, group: string) => {
	let result = null
	switch (group) {
	case "eggcell": {result = row.virus_egg_cell} break
	default: {result = getParticipantsKey(row, group)} break
	}
	return result
}

const createTitreGMTTable = (titreData: any[], groups: string[]) => {

	let titreSummary = summariseAos({
		data: titreData,
		groups: groups,
		defaultCounts: {titres: 0, logtitreSum: 0},
		getKey: getTitreKey,
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

	let tableEl = createTableElementFromAos({
		aos: titreSummary,
		colSpecInit: colSpec,
		title: "GMT",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - SCROLLBAR_WIDTHS[0],
	})

	return tableEl
}

const createTitreGMRTable = (titreData: any[], groups: string[]) => {
	let table = createDiv()

	let ratios = summariseAos({
		data: titreData,
		groups: ["pid"].concat(ALL_GMR_GROUPS),
		defaultCounts: {d0: null, d14: null},
		getKey: getTitreKey,
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

	let tableEl = createTableElementFromAos({
		aos: ratioSummary,
		colSpecInit: colSpec,
		title: "GMR",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - SCROLLBAR_WIDTHS[0],
	})

	return tableEl
}

type Plot<X, Y> = {
	canvas: HTMLCanvasElement,
	renderer: CanvasRenderingContext2D,
	spec: PlotSpec<X, Y>,
	scaleXToPx: (x: X, xFacets: any[]) => number,
	scaleYToPx: (y: Y, yFacets: any[]) => number,
	allXTicksXCoords: number[]
	allYTicksYCoords: number[]
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

const CANVAS_FONT_HEIGHT = 16

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

	renderer.font = `${CANVAS_FONT_HEIGHT}px sans-serif`
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

const arrUnique = <T>(arr: T[]): T[] => {
	const result = Array.from(new Set(arr))
	return result
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

const numberSort = (x: number, y: number) => (x - y)
const generalSort = (x: any, y: any) => (x > y ? 1 : x < y ? -1 : 0)

const desiredOrderSort = (ord: any[]) => {
	return (a: any, b: any) => {
		let result = 0
		let ai = ord.indexOf(a)
		let bi = ord.indexOf(b)
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
	stats: BoxplotStats,
	xCoord: number,
	totalBoxWidth: number,
	color: string,
	altColor: string,
	meanColor: string,
	lineThiccness: number,
) => {

	totalBoxWidth = Math.max(totalBoxWidth, 0)
	let boxWidth = totalBoxWidth / 2
	let meanChonkiness = boxWidth
	let medianChonkiness = boxWidth / 2

	let boxLeft = xCoord - boxWidth
	let boxRight = xCoord

	let boxplotBody = {l: boxLeft, b: stats.q75, r: boxRight, t: stats.q25}
	drawRectOutline(plot.renderer, boxplotBody, color, lineThiccness)
	drawRectOutline(plot.renderer, rectShrink(boxplotBody, lineThiccness), altColor, lineThiccness)

	drawDoubleLine(
		plot.renderer,
		boxLeft - medianChonkiness,
		stats.median,
		boxRight,
		stats.median,
		color,
		altColor,
		lineThiccness,
		[]
	)

	drawDoubleLine(
		plot.renderer,
		boxLeft - meanChonkiness,
		stats.mean,
		boxRight,
		stats.mean,
		meanColor,
		altColor,
		lineThiccness,
		[3, 3]
	)

	drawDoubleLine(
		plot.renderer,
		boxLeft - meanChonkiness / 2,
		stats.mean + stats.meanSe * 1.96,
		boxLeft - meanChonkiness / 2,
		stats.mean - stats.meanSe * 1.96,
		meanColor,
		altColor,
		lineThiccness,
		[3, 3]
	)

	drawDoubleLine(
		plot.renderer,
		xCoord,
		stats.q75,
		xCoord,
		stats.max,
		color,
		altColor,
		lineThiccness,
		[],
		true,
	)

	drawDoubleLine(
		plot.renderer,
		xCoord,
		stats.min,
		xCoord,
		stats.q25,
		color,
		altColor,
		lineThiccness,
		[],
		true,
	)
}

type PlotDimType = "facet" | "plot" | "tick"

type PlotSpec<X, Y> = {
	width: number,
	height: number,
	widthType: PlotDimType,
	heightType: PlotDimType,
	scaleXData?: (x: X) => number,
	scaleYData?: (y: Y) => number,
	padAxis: Rect,
	padData: Rect,
	padFacet: number,
	scaledXMin: number,
	scaledXMax: number,
	yMin: Y,
	yMax: Y,
	xTicks: X[],
	yTicks: Y[],
	xFacetSets: any[][],
	yFacetSets: any[][],
	xLabel: string,
	yLabel: string,
}

const beginPlot = <X, Y>(spec: PlotSpec<X, Y>) => {
	if (spec.widthType === "tick") {
		spec.width = spec.width * spec.xTicks.length + spec.padFacet
		spec.widthType = "facet"
	}
	if (spec.heightType === "tick") {
		spec.height = spec.height * spec.yTicks.length + spec.padFacet
		spec.heightType = "facet"
	}

	if (spec.widthType === "facet") {
		spec.width = spec.width * (spec.xFacetSets.reduce((prev, curr) => prev * curr.length, 1)) +
			spec.padAxis.l + spec.padAxis.r + spec.padData.l + spec.padData.r
	}
	if (spec.heightType === "facet") {
		spec.height = spec.height * (spec.yFacetSets.reduce((prev, curr) => prev * curr.length, 1)) +
			spec.padAxis.t + spec.padAxis.b + spec.padData.t + spec.padData.b
	}

	let canvas = <HTMLCanvasElement>createEl("canvas")
	canvas.width = spec.width
	canvas.height = spec.height

	let renderer = canvas.getContext("2d")!

	let scaleXData = spec.scaleXData ?? ((x) => x as unknown as number)
	let scaleYData = spec.scaleYData ?? ((y) => y as unknown as number)

	const plotMetrics: Rect = {
		t: spec.padAxis.t + spec.padData.t,
		b: spec.height - spec.padAxis.b - spec.padData.b,
		l: spec.padAxis.l + spec.padData.l,
		r: spec.width - spec.padAxis.r - spec.padData.r,
	}

	const getPanelMetrics = (facetSets: any[][], totalRange: number) => {
		const panelsPerInc = [] as number[]
		if (facetSets.length > 0) {
			panelsPerInc[facetSets.length - 1] = 1
		}
		for (let facetSetIndex = facetSets.length - 2; facetSetIndex >= 0; facetSetIndex -= 1) {
			const xFacetSet = facetSets[facetSetIndex]
			const nextIndex = facetSetIndex + 1
			panelsPerInc[facetSetIndex] = facetSets[nextIndex].length * panelsPerInc[nextIndex]
		}

		let totalPanels = 1
		if (facetSets.length > 0) {
			totalPanels = panelsPerInc[0] * facetSets[0].length
		}

		const facetPadTotal = (totalPanels - 1) * spec.padFacet
		const facetRange = (totalRange - facetPadTotal) / totalPanels

		const panelMetrics = {
			panelsPerInc: panelsPerInc,
			totalPanels: totalPanels,
			facetPadTotal: facetPadTotal,
			facetRange: facetRange,
		}
		return panelMetrics
	}

	const xPanelsMetrics = getPanelMetrics(spec.xFacetSets, plotMetrics.r - plotMetrics.l)
	const yPanelsMetrics = getPanelMetrics(spec.yFacetSets, plotMetrics.b - plotMetrics.t)

	const getPanelIndex = (facets: any[], facetSets: any[][], panelsPerInc: number[]) => {
		let panelIndex = 0
		for (let facetIndex = 0; facetIndex < facets.length; facetIndex += 1) {
			const facet = facets[facetIndex]
			const facetSet = facetSets[facetIndex]
			const thisIndex = arrLinSearch(facetSet, facet)
			panelIndex += panelsPerInc[facetIndex] * thisIndex
		}
		return panelIndex
	}

	const scaleXToPx = (val: X, xFacets: any[]) => {
		const panelIndex = getPanelIndex(xFacets, spec.xFacetSets, xPanelsMetrics.panelsPerInc)

		const facetLeft = plotMetrics.l + panelIndex * (xPanelsMetrics.facetRange + spec.padFacet)
		const facetRight = facetLeft + xPanelsMetrics.facetRange

		const result = scale(
			scaleXData(val), spec.scaledXMin, spec.scaledXMax,
			facetLeft, facetRight,
		)
		return result
	}

	const scaleYToPx = (val: Y, yFacets: any[]) => {
		const panelIndex = getPanelIndex(yFacets, spec.yFacetSets, yPanelsMetrics.panelsPerInc)

		const facetTop = plotMetrics.t + panelIndex * (yPanelsMetrics.facetRange + spec.padFacet)
		const facetBottom = facetTop + yPanelsMetrics.facetRange

		let result = scale(
			scaleYData(val), scaleYData(spec.yMin), scaleYData(spec.yMax),
			facetBottom, facetTop,
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

	// NOTE(sen) Axis labels

	let axisTextCol = axisCol
	drawText(
		renderer, spec.xLabel,
		(plotMetrics.r - plotMetrics.l) / 2 + plotMetrics.l, spec.height - 3,
		axisTextCol, 0, "bottom", "center",
	)
	drawText(
		renderer, spec.yLabel,
		3, (plotMetrics.b - plotMetrics.t) / 2 + plotMetrics.t,
		axisTextCol, -90, "top", "center",
	)

	// NOTE(sen) Ticks and grid

	let tickLength = 5
	let tickToText = 5

	type FacetIter = {
		facetIndices: number[],
		done: boolean,
		facetSets: any[][],
	}

	const beginFacetIteration = (facetSets: any[][]): FacetIter => {
		let facetIndices = [] as number[]
		for (let facetIndex = 0; facetIndex < facetSets.length; facetIndex += 1) {
			facetIndices.push(0)
		}
		return {
			facetIndices: facetIndices,
			done: false,
			facetSets: facetSets,
		}
	}

	const getCurrentFacetValues = (iter: FacetIter) => {
		let facets = [] as any[]
		for (let facetSetIndex = 0; facetSetIndex < iter.facetSets.length; facetSetIndex += 1) {
			const setValueIndex = iter.facetIndices[facetSetIndex]
			facets.push(iter.facetSets[facetSetIndex][setValueIndex])
		}
		return facets
	}

	const nextFacet = (iter: FacetIter) => {
		let facetCurrentSetIndex = iter.facetIndices.length - 1
		while (true) {
			if (facetCurrentSetIndex == -1) {
				iter.done = true
				break
			}
			if (iter.facetIndices[facetCurrentSetIndex] >= iter.facetSets[facetCurrentSetIndex].length - 1) {
				iter.facetIndices[facetCurrentSetIndex] = 0
				facetCurrentSetIndex -= 1
			} else {
				iter.facetIndices[facetCurrentSetIndex] += 1
				break
			}
		}
	}

	let allXTicksXCoords = []
	for (const xFacetIter = beginFacetIteration(spec.xFacetSets); !xFacetIter.done; nextFacet(xFacetIter)) {
		let xFacets = getCurrentFacetValues(xFacetIter)

		for (let xTick of spec.xTicks) {
			let xCoord = scaleXToPx(xTick, xFacets)
			allXTicksXCoords.push(xCoord)
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
	}

	const gridCol = axisCol + "22"
	const gridThiccness = 1

	let allYTicksYCoords = []
	for (const yFacetIter = beginFacetIteration(spec.yFacetSets); !yFacetIter.done; nextFacet(yFacetIter)) {
		let yFacets = getCurrentFacetValues(yFacetIter)

		for (let yTick of spec.yTicks) {
			let yCoord = scaleYToPx(yTick, yFacets)
			allYTicksYCoords.push(yCoord)
			drawRect(
				renderer,
				{l: spec.padAxis.l - tickLength, r: spec.padAxis.l,
					t: yCoord - axisThiccness, b: yCoord},
				axisCol
			)
			drawLine(
				renderer,
				spec.padAxis.l, yCoord, spec.width - spec.padAxis.r, yCoord,
				gridCol, gridThiccness, [],
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
	}

	// NOTE(sen) Facet labels and separators
	const facetSepColor = "#555555"
	const facetSepThiccness = 1
	let repeatLabels = 1
	for (let facetSetIndex = 0; facetSetIndex < spec.xFacetSets.length; facetSetIndex += 1) {

		const facetSetValues = spec.xFacetSets[facetSetIndex]
		const panelsPerInc = xPanelsMetrics.panelsPerInc[facetSetIndex]
		const facetValueSpan = panelsPerInc * xPanelsMetrics.facetRange + (panelsPerInc - 1) * spec.padFacet
		const yOffset = facetSetIndex * CANVAS_FONT_HEIGHT + 3
		const sepThiccness = facetSepThiccness * (spec.xFacetSets.length - facetSetIndex)
		let xOffset = plotMetrics.l

		for (let repeatIndex = 0; repeatIndex < repeatLabels; repeatIndex += 1) {
			for (let setValueIndex = 0; setValueIndex < facetSetValues.length; setValueIndex += 1) {

				const xFacet = facetSetValues[setValueIndex]
				const facetStart = xOffset + setValueIndex * (facetValueSpan + spec.padFacet)
				const facetCenter = facetStart + facetValueSpan / 2
				const facetGap = facetStart + facetValueSpan + spec.padFacet / 2
				drawText(renderer, `${xFacet ?? MISSING_STRING}`, facetCenter, yOffset, axisTextCol, 0, "top", "center")
				if (setValueIndex < facetSetValues.length - 1) {
					drawLine(
						renderer, facetGap, yOffset, facetGap, spec.height - spec.padAxis.b - axisThiccness,
						facetSepColor, sepThiccness, [],
					)
				}
			}
			xOffset += (facetValueSpan + spec.padFacet) * facetSetValues.length
		}
		repeatLabels *= facetSetValues.length
	}

	repeatLabels = 1
	for (let facetSetIndex = 0; facetSetIndex < spec.yFacetSets.length; facetSetIndex += 1) {

		const facetSetValues = spec.yFacetSets[facetSetIndex]
		const panelsPerInc = yPanelsMetrics.panelsPerInc[facetSetIndex]
		const facetValueSpan = panelsPerInc * yPanelsMetrics.facetRange + (panelsPerInc - 1) * spec.padFacet
		const xOffset = facetSetIndex * CANVAS_FONT_HEIGHT + 3
		const sepThiccness = facetSepThiccness * (spec.xFacetSets.length - facetSetIndex)
		let yOffset = plotMetrics.t

		for (let repeatIndex = 0; repeatIndex < repeatLabels; repeatIndex += 1) {
			for (let setValueIndex = 0; setValueIndex < facetSetValues.length; setValueIndex += 1) {

				const yFacet = facetSetValues[setValueIndex]
				const facetStart = yOffset + setValueIndex * (facetValueSpan + spec.padFacet)
				const facetCenter = facetStart + facetValueSpan / 2
				const facetGap = facetStart + facetValueSpan + spec.padFacet / 2
				drawText(renderer, `${yFacet ?? MISSING_STRING}`, spec.width - xOffset, facetCenter, axisTextCol, 90, "top", "center")
				drawText(renderer, `${yFacet ?? MISSING_STRING}`, spec.padAxis.l + xOffset, facetCenter, axisTextCol, -90, "top", "center")
				if (setValueIndex < facetSetValues.length - 1) {
					drawLine(
						renderer, spec.width - xOffset, facetGap, spec.padAxis.l + xOffset, facetGap,
						facetSepColor, sepThiccness, [],
					)
				}
			}
			yOffset += (facetValueSpan + spec.padFacet) * facetSetValues.length
		}
		repeatLabels *= facetSetValues.length
	}

	let result: Plot<X, Y> = {canvas: canvas, renderer: renderer, spec: spec,
		scaleXToPx: scaleXToPx, scaleYToPx: scaleYToPx,
		allXTicksXCoords: allXTicksXCoords, allYTicksYCoords: allYTicksYCoords}

	return result
}

const createTitrePlot = (data: any[], settings: TitresSettings) => {
	let container = createDiv()
	container.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX + SCROLLBAR_WIDTHS[1]}px)`
	container.style.overflowX = "scroll"

	let allTitres = [5, 10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240]

	const collectFacetSet = (facet: any) => {
		let result = arrUnique(data.map(x => (getTitreKey(x, facet)))).sort(generalSort)
		switch (facet) {
		case "subtype": result = result.sort(desiredOrderSort(["H1", "H3", "BVic", "BYam"])); break
		}
		return result
	}

	const collectFacetSets = (facets: TitreFacets[]) => {
		const facetValues: any[][] = []
		if (facets.length > 0) {
			for (let facetIndex = 0; facetIndex < facets.length; facetIndex += 1) {
				const facet = facets[facetIndex]
				facetValues[facetIndex] = collectFacetSet(facet)
			}
		}
		return facetValues
	}

	let xFacetSets = collectFacetSets(settings.xFacets)
	let yFacetSets = collectFacetSets(settings.yFacets)
	const xAxisTicks = collectFacetSet(settings.xAxis)

	let plot = beginPlot({
		width: 200,
		widthType: "tick",
		height: 400,
		heightType: "facet",
		padAxis: {l: 70, r: 10, t: 30, b: 50},
		padData: {l: 50, r: 50, t: 10, b: 10},
		padFacet: 40,
		scaledXMin: -0.5,
		scaledXMax: settings.xAxis === "day" ? xAxisTicks.length + 0.5 : xAxisTicks.length - 0.5,
		yMin: 5,
		yMax: 10240,
		scaleYData: Math.log,
		scaleXData: (x) => {
			let result = xAxisTicks.indexOf(x)
			switch (settings.xAxis) {
			case "day": if (x >= 220) {result += 1}
			}
			return result
		},
		yTicks: allTitres,
		xTicks: xAxisTicks,
		xFacetSets: xFacetSets,
		yFacetSets: yFacetSets,
		xLabel: settings.xAxis,
		yLabel: "Titre",
	})

	const titresWide = summariseAos({
		data: data,
		groups: ALL_GMT_GROUPS.concat("pid").filter(x => x !== "day" && x !== "year"),
		defaultCounts: {y2020day0: null, y2020day7: null, y2020day14: null, y2020day220: null,
			y2021day0: null, y2021day7: null, y2021day14: null, y2021day220: null},
		getKey: getTitreKey,
		addRow: (row, titres) => {
			const colname = `y${row.year}day${row.day}`
			//@ts-ignore
			if (titres[colname] !== undefined) {
				//@ts-ignore
				titres[colname] = row.titre
			}
		},
	})

	const getLocationInfo = (row: any) => {
		let result = null
		if (row.titre !== null) {
			const rowYFacets = settings.yFacets.map(yFacet => getTitreKey(row, yFacet))
			const rowXFacets = settings.xFacets.map(xFacet => getTitreKey(row, xFacet))
			const facetID = rowXFacets.concat(rowYFacets).join("_")

			const rowXAxis = getTitreKey(row, settings.xAxis)
			const stripID = facetID + "__" + rowXAxis
			const regionID = stripID + "___" + row.titre

			result = {xFacets: rowXFacets, yFacets: rowYFacets, xAxis: rowXAxis,
				facetID: facetID, stripID: stripID, regionID: regionID}
		}
		return result
	}

	const addCountToMap = (counts: any, key: any) => {
		if (counts[key] === undefined) {
			counts[key] = 0
		}
		counts[key] += 1
	}

	const plotCounts: any = {}
	const idStripCounts: any = {}
	for (let row of data) {
		const info = getLocationInfo(row)
		if (info !== null) {
			addCountToMap(plotCounts, info.facetID)
			addCountToMap(plotCounts, info.stripID)
			addCountToMap(plotCounts, info.regionID)

			if (idStripCounts[row.pid] === undefined) {
				idStripCounts[row.pid] = {}
			}
			addCountToMap(idStripCounts[row.pid], info.stripID)
		}
	}

	// NOTE(sen) Points
	for (let row of data) {
		const rowLocationInfo = getLocationInfo(row)
		if (rowLocationInfo !== null) {
			const countInStrip = plotCounts[rowLocationInfo.stripID]

			let lineAlphaNumber = 0
			if (countInStrip > 5000) {
				lineAlphaNumber = 1
			} else if (countInStrip > 2000) {
				lineAlphaNumber = 3
			} else if (countInStrip > 1000) {
				lineAlphaNumber = 5
			} else if (countInStrip > 500) {
				lineAlphaNumber = 10
			} else if (countInStrip > 300) {
				lineAlphaNumber = 40
			} else if (countInStrip > 100) {
				lineAlphaNumber = 70
			} else if (countInStrip > 50) {
				lineAlphaNumber = 85
			} else if (countInStrip > 10) {
				lineAlphaNumber = 100
			} else {
				lineAlphaNumber = 200
			}

			const lineAlpha = lineAlphaNumber.toString(16).padStart(2, "0")
			const lineColBase = "#61de2a"
			const lineCol = lineColBase + lineAlpha
			const pointAlpha = lineAlpha

			// TODO(sen) Have jitter be the same per individual
			let yJitter = 0
			let xJitter = 0

			let yCoord = plot.scaleYToPx(row.titre, rowLocationInfo.yFacets)
			let xCoord = plot.scaleXToPx(rowLocationInfo.xAxis, rowLocationInfo.xFacets)
			let pointSize = 5
			let pointHalfSize = pointSize / 2
			let pointCol = lineColBase + pointAlpha
			drawRect(
				plot.renderer,
				{l: xCoord - pointHalfSize, r: xCoord + pointHalfSize,
					t: yCoord - pointHalfSize, b: yCoord + pointHalfSize},
				pointCol
			)
		}
	}

	let dayPxStep = plot.allXTicksXCoords[1] - plot.allXTicksXCoords[0]
	let titrePxStep = plot.allYTicksYCoords[0] - plot.allYTicksYCoords[1]

	let boxLineThiccness = 2
	let boxplotCol = "#ffa600"
	let distColor = "#de61a8"
	let altColor = "#000000"

	let gapX = 10
	let boxWidth = dayPxStep / 2 - gapX
	let distWidth = boxWidth

	let boxplotMeanCol = boxplotCol

	// TODO(sen) Lines

	if (boxWidth >= 15) {

		let summary = summariseAos({
			data: data,
			groups: settings.xFacets.concat(settings.yFacets).concat([settings.xAxis]),
			defaultCounts: () => ({
				titres: [] as number[],
				titreCounts: allTitres.reduce((acc, titre) => {acc[titre] = 0; return acc}, {} as any)
			}),
			getKey: getTitreKey,
			addRow: (row, summ) => {
				summ.titres.push(row.titre)
				if (summ.titreCounts[row.titre] === undefined) {
					summ.titreCounts[row.titre] = 0
				}
				summ.titreCounts[row.titre] += 1
			}
		})

		for (let summaryRow of summary) {
			if (summaryRow.titres.length >= 10) {
				const xVal = summaryRow[settings.xAxis]
				const xFacets = settings.xFacets.map(xFacet => summaryRow[xFacet])
				const xCoord = plot.scaleXToPx(xVal, xFacets)

				const yFacets = settings.yFacets.map(yFacet => summaryRow[yFacet])
				const yVals = summaryRow.titres.map((titre: number) => plot.scaleYToPx(titre, yFacets))

				const stats = getSortedStats(yVals)

				if (stats !== null) {
					addBoxplot(
						plot, stats, xCoord,
						boxWidth, boxplotCol, altColor, boxplotMeanCol, boxLineThiccness
					)
				}

				const titreCountMax = arrMax(Object.values(summaryRow.titreCounts))
				const titresSorted = Object.keys(summaryRow.titreCounts).map(x => parseInt(x)).sort((a, b) => a - b)
				const titreCounts01: any = []
				for (let key of titresSorted) {
					titreCounts01.push(summaryRow.titreCounts[key] / titreCountMax)
				}

				const firstNonZero = titreCounts01.findIndex((x: any) => x !== 0)
				const lastNonZero = titreCounts01.findLastIndex((x: any) => x !== 0)

				let prevBarRight = null
				for (let count01Index = firstNonZero; count01Index <= lastNonZero; count01Index += 1) {
					let count01 = titreCounts01[count01Index]
					let titre = titresSorted[count01Index]
					let count = summaryRow.titreCounts[titre]
					let yCoord = plot.scaleYToPx(titre, yFacets)

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
						const hlineY = yCoord + down
						drawLine(
							plot.renderer, vLeft - halfThicc, hlineY, vRight + halfThicc, hlineY,
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
		}
	}

	addEl(container, plot.canvas as HTMLElement)
	return container
}

const createSurveyTable = (completions: { [key: string]: number[] }, data: Data, year: number) => {
	let tableContainer = createDiv()

	let tableEl = createTableElementFromAos({
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

	addEl(tableContainer, tableEl)
	return tableContainer
}

const createCompletionsTable = (completions: { [key: string]: number[] }) => {

	let collapsed = []
	for (let [pid, completedSurveys] of Object.entries(completions)) {
		completedSurveys = completedSurveys.sort((a, b) => a - b)
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

	let tableEl = createTableElementFromAos({
		aos: collapsed,
		colSpecInit: { pid: {}, completions: { width: 300 } },
		title: "Collapsed completions",
	})

	let tableContainer = createDiv()
	addEl(tableContainer, tableEl)

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

	return { success: success, data: <Data>data }
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
	xFacets: TitreFacets[],
	yFacets: TitreFacets[],
	xAxis: TitreFacets,
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
	let xFacets = `xFacets=${settings.xFacets.join(",")}`
	let yFacets = `yFacets=${settings.yFacets.join(",")}`
	let result = `titres?${groupsGMTs}&${groupsGMRs}&${xFacets}&${yFacets}&xAxis=${settings.xAxis}`
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
	let urlXFacets = def.xFacets
	let urlYFacets = def.yFacets
	let urlXAxis = def.xAxis

	if (window.location.pathname === "/titres") {
		let params = new URLSearchParams(window.location.search)

		let gmtGroupsRes = getURLArrayParam(params, "groupsGMTs", ALL_GMT_GROUPS, urlGroupsGMTs)
		urlGroupsGMTs = gmtGroupsRes.urlArr

		let gmrGroupsRes = getURLArrayParam(params, "groupsGMRs", ALL_GMT_GROUPS, urlGroupsGMRs)
		urlGroupsGMRs = gmrGroupsRes.urlArr

		let xFacetsRes = getURLArrayParam(params, "xFacets", ALL_TITRE_FACETS, urlXFacets)
		urlXFacets = xFacetsRes.urlArr

		let yFacetsRes = getURLArrayParam(params, "yFacets", ALL_TITRE_FACETS, urlYFacets)
		urlYFacets = yFacetsRes.urlArr

		let xAxisRes = getURLSingleParam(params, "xAxis", ALL_TITRE_FACETS, urlXAxis)
		urlXAxis = xAxisRes.urlParam

		let needToFixAddress = gmtGroupsRes.needToFixAddress || gmrGroupsRes.needToFixAddress ||
			xFacetsRes.needToFixAddress || yFacetsRes.needToFixAddress || xAxisRes.needToFixAddress
		if (needToFixAddress) {
			window.history.replaceState(null, "", getTitresPageURL({
				groupsGMTs: urlGroupsGMTs,
				groupsGMRs: urlGroupsGMRs,
				xFacets: urlXFacets,
				yFacets: urlYFacets,
				xAxis: urlXAxis,
			}))
		}
	}

	let result: TitresSettings = {
		groupsGMTs: urlGroupsGMTs,
		groupsGMRs: urlGroupsGMRs,
		xFacets: urlXFacets,
		yFacets: urlYFacets,
		xAxis: urlXAxis,
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

const createLoadingPage = () => {
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

const createPasswordPage = (onValidPassword: (data: Data) => void) => {
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
					onValidPassword(fetchResult.data)
				}
				buttonText.textContent = "Submit"
			}
		}
	})

	return container
}

type Data = {
	participants: any[],
	withdrawn: any[],
	bleed_dates: any[],
	consent: any[]
	weekly_surveys: any[],
	postinf_bleed_dates: any[],
	titres: any[],
	consent_conflicts: any[],
	consent_multiple: any[],
	covid_bleeds_no_vax: any[],
	missing_baseline: any[],
	missing_bleed_dates: any[],
	missing_vaccination_history: any[],
	missing_vaccination_records: any[],
	swabs_missing_date: any[],
	withdrawn_missing_date: any[],
}

type Pages = {
	participants: ParticipantsSettings,
	"weekly-surveys": WeeklySurveySettings,
	bleeds: BleedsSettings,
	counts: CountsSettings,
	titres: TitresSettings,
	problems: ProblemsSettings,
}

type ProblemsSettings = {}
type ParticipantsSettings = {}
type WeeklySurveySettings = {year: YearID}
type BleedsSettings = {year: YearID}

const updatePageFromURL = (pages: Pages): DataPageID => {
	let page = getDataPageFromURL()
	switch (page) {
	case "participants": pages.participants = {}; break
	case "problems": pages.problems = {}; break
	case "counts": pages.counts = getCountSettingsFromURL(pages.counts); break
	case "bleeds": pages.bleeds.year = getBleedsYearFromURL(pages.bleeds.year); break
	case "weekly-surveys": pages["weekly-surveys"].year = getSurveysYearFromURL(pages["weekly-surveys"].year); break
	case "titres": pages.titres = getTitresSettingsFromURL(pages.titres); break
	default: console.error("unexpected page:", page); page = "counts"; break
	}
	return page
}

const createSidebar = (
	activePage: DataPageID,
	pageSpecific: HTMLElement,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void,
) => {
	const sidebar = createDiv()
	sidebar.style.width = SIDEBAR_WIDTH_PX + "px"
	sidebar.style.height = "100vh"
	sidebar.style.flexShrink = "0"
	sidebar.style.display = "flex"
	sidebar.style.flexDirection = "column"
	sidebar.style.justifyContent = "space-between"
	sidebar.style.overflowX = "hidden"
	sidebar.style.overflowY = "scroll"

	const top = addDiv(sidebar)

	const linksContainer = addEl(top, createSwitch(activePage, ALL_DATAPAGE_IDS, onDatapageChange))
	linksContainer.style.marginBottom = "20px"

	addEl(top, pageSpecific)

	let bottom = addDiv(sidebar)
	let logout = addDiv(bottom)
	logout.textContent = "Logout"
	logout.style.cursor = "pointer"
	logout.style.marginTop = "20px"
	logout.addEventListener("mouseover", (event) => logout.style.backgroundColor = "var(--color-selected)")
	logout.addEventListener("mouseleave", (event) => logout.style.backgroundColor = "inherit")
	logout.addEventListener("click", (event) => {
		localStorage.removeItem("password")
		onLogout()
	})

	return sidebar
}

const createDatapageContainer = () => {
	const container = createDiv()
	container.style.display = "flex"
	return container
}

const createParticipantsPage = (data: Data, onDatapageChange: (page: DataPageID) => void, onLogout: () => void) => {
	const page = createDatapageContainer()
	const settings = createDiv()

	addEl(page, createSidebar("participants", settings, onDatapageChange, onLogout))

	let container = addDiv(page)
	container.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	let table = addDiv(container)
	table.style.maxWidth = container.style.width

	let tableEl = createTableElementFromAos({
		aos: data.participants,
		colSpecInit: {
			pid: {},
			site: {},
			email: { width: 450, format: (x) => x?.toLowerCase(), filterValProcess: (val: string) => val.toLowerCase() },
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
	addEl(table, tableEl)

	return page
}

const createCountsTable = (data: Data, settings: CountsSettings) => {
	let tableEl = createDiv()
	switch (settings.table) {
		case "records": {
			tableEl = createCountsRecordsTable(data, settings.groupsRecords)
		} break;

		case "routine-bleeds": {
			tableEl = createCountsBleedsTable(data, settings.groupsBleeds)
		} break;

		case "postinfection-bleeds": {
			tableEl = createCountsPostinfBleedsTable(data, settings.groupsPostinfBleeds)
		} break;

		default: console.error("unexpected counts table name", settings.table)
	}
	return tableEl
}

const createCountsSwitch = (parent: HTMLElement, data: Data, settings: CountsSettings) => {
	let switchEl = createDiv()
	let createTable = () => createDiv()

	switch (settings.table) {
		case "records": {
			createTable = () => createCountsRecordsTable(data, settings.groupsRecords)
			switchEl = createSwitch(
				settings.groupsRecords,
				<RecordGroups[]>ALL_RECORD_GROUPS,
				(groups) => {
					settings.groupsRecords = groups
					window.history.pushState(null, "", getCountsPageURL(settings))
					replaceChildren(parent, createTable())
				},
			)
		} break;

		case "routine-bleeds": {
			createTable = () => createCountsBleedsTable(data, settings.groupsRecords)
			switchEl = createSwitch(
				settings.groupsBleeds,
				<BleedsGroups[]>ALL_BLEEDS_GROUPS,
				(groups) => {
					settings.groupsBleeds = groups
					window.history.pushState(null, "", getCountsPageURL(settings))
					replaceChildren(parent, createTable())
				},
			)
		} break;

		case "postinfection-bleeds": {
			createTable = () => createCountsPostinfBleedsTable(data, settings.groupsRecords)
			switchEl = createSwitch(
				settings.groupsPostinfBleeds,
				<PostinfBleedsGroups[]>ALL_POSTINF_BLEEDS_GROUPS,
				(groups) => {
					settings.groupsPostinfBleeds = groups
					window.history.pushState(null, "", getCountsPageURL(settings))
					replaceChildren(parent, createTable())
				},
			)
		} break;

		default: console.error("unexpected counts table name", settings.table)
	}

	return switchEl
}

const createCountsPage = (
	data: Data,
	settings: CountsSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void,
) => {
	const counts = createDiv()
	counts.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	counts.style.overflowX = "hidden"
	counts.style.display = "flex"

	let tableParent = addDiv(counts)
	tableParent.style.maxWidth = counts.style.width
	addEl(tableParent, createCountsTable(data, settings))

	const switchParent = createDiv()
	addEl(switchParent, createCountsSwitch(tableParent, data, settings))

	const updateTableAndSwitch = () => {
		replaceChildren(tableParent, createCountsTable(data, settings))
		replaceChildren(switchParent, createCountsSwitch(tableParent, data, settings))
	}

	let switchContainer = createDiv()

	let tableSwitch = addEl(switchContainer, createSwitch(
		settings.table,
		<CountTableID[]>ALL_COUNTS_TABLES,
		(table) => {
			settings.table = table
			window.history.pushState(null, "", getCountsPageURL(settings))
			updateTableAndSwitch()
		},
	))
	tableSwitch.style.marginBottom = "20px"

	addEl(switchContainer, switchParent)

	const page = createDatapageContainer()
	addEl(page, createSidebar("counts", switchContainer, onDatapageChange, onLogout))
	addEl(page, counts)

	return page
}

const createBleedsPage = (
	data: Data,
	settings: BleedsSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void,
) => {
	const bleeds = createDiv()
	bleeds.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	bleeds.style.display = "flex"

	const tableParent = addDiv(bleeds)
	tableParent.style.maxWidth = bleeds.style.width

	addEl(tableParent, createBleedsTable(DOWNLOAD_CSV, data, settings.year))

	const yearSwitch = createSwitch (
		settings.year, YEARS,
		(year) => {
			settings.year = year
			window.history.pushState(null, "", `bleeds?year=${year}`)
			replaceChildren(tableParent, createBleedsTable(DOWNLOAD_CSV, data, settings.year))
		},
	)

	const page = createDatapageContainer()
	addEl(page, createSidebar("bleeds", yearSwitch, onDatapageChange, onLogout))
	addEl(page, bleeds)

	return page
}

const createWeeklySurveysPage = (
	data: Data,
	settings: WeeklySurveySettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void,
) => {
	let hscrollContainer = createDiv()
	hscrollContainer.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
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

	let surveyDates = {
		2020: createTableElementFromAos({
			aos: getDates(32, "2020-04-06", "2020-04-12", "2020-04-13"),
			colSpecInit: {week: {}, start: {}, end: {}, send: {}},
			title: "Weekly survey dates 2020",
		}),

		2021: createTableElementFromAos({
			aos: getDates(52, "2021-01-04", "2021-01-10", "2021-01-11"),
			colSpecInit: {week: {}, start: {}, end: {}, send: {}},
			title: "Weekly survey dates 2021",
		}),

		2022: createTableElementFromAos({
			aos: getDates(52, "2022-01-03", "2022-01-09", "2022-01-10"),
			colSpecInit: {week: {}, start: {}, end: {}, send: {}},
			title: "Weekly survey dates 2022",
		})
	}

	const datesTableParent = addDiv(container)
	const surveysTableParent = addDiv(container)
	const completionsTableParent = addDiv(container)

	const updateTables = (year: YearID) => {
		replaceChildren(datesTableParent, surveyDates[year])
		const completions = {}
		replaceChildren(surveysTableParent, createSurveyTable(completions, data, year))
		replaceChildren(completionsTableParent, createCompletionsTable(completions))
	}

	updateTables(settings.year)

	const yearSwitch = createSwitch (
		settings.year, YEARS,
		(year) => {
			settings.year = year
			window.history.pushState(null, "", `weekly-surveys?year=${year}`)
			updateTables(year)
		},
	)

	const page = createDatapageContainer()
	addEl(page, createSidebar("weekly-surveys", yearSwitch, onDatapageChange, onLogout))
	addEl(page, hscrollContainer)

	return page
}

const createTitresPage = (
	data: Data,
	settings: TitresSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	let container2 = createDiv()
	container2.style.overflowX = "hidden"
	container2.style.overflowY = "scroll"
	container2.style.height = "100vh"
	container2.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`

	let help = addDiv(container2)

	const fillHelp = () => {
		removeChildren(help)
		addTextline(help, "GMT, GMR tables and the titre plot only use the data displayed in the Titres table (so you can filter the titres table and change everything else on the page).")
		addTextline(help, "Boxplots: minimum - quartile 25 - quartile 75 - maximum. Solid midline: median. Dashed midline: mean (vertical dashed line - 95% CI for mean). Right side: histogram. Numbers: titre measurement counts.")
		//@ts-ignore
		addGroupsExplanatoryNotes(help, arrUnique(settings.groupsGMRs.concat(settings.groupsGMTs).concat(settings.xFacets).concat(settings.yFacets)))
	}
	fillHelp()

	let top = addDiv(container2)
	top.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX + SCROLLBAR_WIDTHS[1]}px)`
	top.style.flex = "1 0"
	top.style.display = "flex"
	top.style.overflow = "hidden"
	let bottom = addEl(container2, <HTMLElement>top.cloneNode(true))

	let left = addDiv(top)
	left.style.maxWidth = `calc((100vw - ${SIDEBAR_WIDTH_PX + SCROLLBAR_WIDTHS[1]}px) / 2)`
	left.style.flex = "1 0"
	left.style.overflow = "hidden"
	let right = addEl(top, <HTMLElement>left.cloneNode(true))

	let tableParent = addDiv(left)
	tableParent.style.flex = "1 0"
	tableParent.style.display = "flex"

	let plotParent = addEl(bottom, <HTMLElement>tableParent.cloneNode(true))

	let gmtTableParent = addEl(right, <HTMLElement>tableParent.cloneNode(true))
	gmtTableParent.style.height = "50%"

	let gmrTableParent = addEl(right, <HTMLElement>tableParent.cloneNode(true))

	let latestFilteredData: any[] = []

	const updateGMTs = () => replaceChildren(gmtTableParent, createTitreGMTTable(latestFilteredData, settings.groupsGMTs))
	const updateGMRs = () => replaceChildren(gmrTableParent, createTitreGMRTable(latestFilteredData, settings.groupsGMRs))
	const updatePlot = () => replaceChildren(plotParent, createTitrePlot(latestFilteredData, settings))

	addEl(tableParent, createTitreTable(data, (filteredData) => {
		latestFilteredData = filteredData
		updateGMTs()
		updateGMRs()
		updatePlot()
	}))

	let settingsEl = createDiv()

	let gmtGroupsSwitchLabel = addDiv(settingsEl)
	gmtGroupsSwitchLabel.textContent = "GMT groups"

	let gmtGroupsSwitch = createSwitch(
		settings.groupsGMTs,
		<GMTGroups[]>ALL_GMT_GROUPS,
		(groups) => {
			settings.groupsGMTs = groups
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updateGMTs()
		},
	)

	addEl(settingsEl, gmtGroupsSwitch)

	let gmrGroupsSwitchLabel = addDiv(settingsEl)
	gmrGroupsSwitchLabel.textContent = "GMR groups"
	gmrGroupsSwitchLabel.style.marginTop = "30px"

	let gmrGroupsSwitch = createSwitch(
		settings.groupsGMRs,
		<GMRGroups[]>ALL_GMR_GROUPS,
		(groups) => {
			settings.groupsGMRs = groups
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updateGMRs()
		},
	)

	addEl(settingsEl, gmrGroupsSwitch)

	let xAxisSwitchLabel = addDiv(settingsEl)
	xAxisSwitchLabel.textContent = "X axis"
	xAxisSwitchLabel.style.marginTop = "30px"

	let xAxisSwitch = createSwitch(
		settings.xAxis,
		<TitreFacets[]>ALL_TITRE_FACETS,
		(groups) => {
			settings.xAxis = groups
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
	)

	addEl(settingsEl, xAxisSwitch)

	let xFacetsSwitchLabel = addDiv(settingsEl)
	xFacetsSwitchLabel.textContent = "X facets"
	xFacetsSwitchLabel.style.marginTop = "30px"

	let xFacetsSwitch = createSwitch(
		settings.xFacets,
		<TitreFacets[]>ALL_TITRE_FACETS,
		(groups) => {
			settings.xFacets = groups
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
	)

	addEl(settingsEl, xFacetsSwitch)

	let yFacetsSwitchLabel = addDiv(settingsEl)
	yFacetsSwitchLabel.textContent = "Y facets"
	yFacetsSwitchLabel.style.marginTop = "30px"

	let yFacetsSwitch = createSwitch(
		settings.yFacets,
		<TitreFacets[]>ALL_TITRE_FACETS,
		(groups) => {
			settings.yFacets = groups
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
	)

	addEl(settingsEl, yFacetsSwitch)

	const page = createDatapageContainer()
	addEl(page, createSidebar("titres", settingsEl, onDatapageChange, onLogout))
	addEl(page, container2)

	return page
}

const createProblemsPage = (data: Data, onDatapageChange: (page: DataPageID) => void, onLogout: () => void) => {
	const container = createDiv()
	container.style.maxHeight = "100vh"
	container.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	container.style.overflowY = "scroll"

	const helpEl = addDiv(container)
	addTextline(helpEl, "If a table is missing then no problems were found. Let me know (sen.khvorov@unimelb.edu.au) of any false flags.")
	addTextline(helpEl, "Missing data - fill if able, leave missing if not.")
	addTextline(helpEl, "List of tables:")
	addTextline(helpEl, "Consent conflicts: Multiple consent forms filled and information on study group conflicts. We don't need the manual form in the presence of the electronic one. You can fix the wrong form or delete it.")
	addTextline(helpEl, "Covid bleed no vax: Covid bleed recorded but no covid vaccinations recorded.")
	addTextline(helpEl, "Missing baseline: baseline survey")
	addTextline(helpEl, "Missing bleed dates: routing bleed dates missing (but we have titres)")
	addTextline(helpEl, "Missing vaccination history: screening form")
	addTextline(helpEl, "Missing vaccination records: Missing study year vaccination records (vaccination form)")
	addTextline(helpEl, "Swabs missing date: missing swab dates")
	addTextline(helpEl, "Withdrawn missing dates: missing withdrawal dates")

	const tablesContainer = addDiv(container)
	tablesContainer.style.display = "flex"
	tablesContainer.style.flexWrap = "wrap"

	if (data.consent_conflicts.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.consent_conflicts,
			colSpecInit: {
				pid: {}, year: {}, site: {},
				fluManual: {access: "flu_manual"},
				fluElecVac: {access: "flu_electronic_vac"},
				fluElecUnvac: {access: "flu_electronic_unvac"},
				covidManual: {access: "covid_manual"},
				covidElec: {access: "covid_electronic"},
			},
			title: "Consent conflicts",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.covid_bleeds_no_vax.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.covid_bleeds_no_vax,
			colSpecInit: {
				pid: {}, year: {}, site: {},
				day: {}, date: {}
			},
			title: "Covid bleeds no vax",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.missing_baseline.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.missing_baseline,
			colSpecInit: {
				pid: {}, site: {}, gender: {}, dob: {}, atsi: {}, date_screening: {}, email: {width: 300}, mobile: {},
				recruited: {access: "recruitment_year"}, age_screening: {}
			},
			title: "Missing baseline",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.missing_bleed_dates.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.missing_bleed_dates,
			colSpecInit: {
				pid: {}, site: {}, year: {}, day: {}, date: {}, virus: {}, titre: {}, subtype: {},
				eggcell: {access: "virus_egg_cell"},
			},
			title: "Missing bleed dates",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.missing_vaccination_history.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.missing_vaccination_history,
			colSpecInit: {
				pid: {}, site: {}, year: {access: "recruitment_year"},
			},
			title: "Missing vaccination history",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.missing_vaccination_records.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.missing_vaccination_records,
			colSpecInit: {
				pid: {}, site: {}, year: {access: "recruitment_year"},
			},
			title: "Missing vaccination records",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.swabs_missing_date.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.swabs_missing_date,
			colSpecInit: {
				pid: {}, site: {}, year: {}, date: {access: "samp_date"}, viruses: {}
			},
			title: "Swabs missing date",
			getTableHeightInit: () => 500,
		}))
	}

	if (data.withdrawn_missing_date.length > 0) {
		addEl(tablesContainer, createTableElementFromAos({
			aos: data.withdrawn_missing_date,
			colSpecInit: {
				pid: {}, site: {}, year: {access: "redcap_project_year"}, date: {access: "withdrawal_date"},
			},
			title: "Withdrawn missing date",
			getTableHeightInit: () => 500,
		}))
	}

	const page = createDatapageContainer()
	addEl(page, createSidebar("problems", createDiv(), onDatapageChange, onLogout))
	addEl(page, container)

	return page
}

const createDatapage = (page: DataPageID, pages: Pages, data: Data, onDatapageChange: (page: DataPageID) => void, onLogout: () => void) => {
	clearPageListners()
	var pageEl = createDiv()
	switch (page) {
	case "participants": pageEl = createParticipantsPage(data, onDatapageChange, onLogout); break
	case "counts": pageEl = createCountsPage(data, pages.counts, onDatapageChange, onLogout); break
	case "bleeds": pageEl = createBleedsPage(data, pages.bleeds, onDatapageChange, onLogout); break
	case "weekly-surveys": pageEl = createWeeklySurveysPage(data, pages["weekly-surveys"], onDatapageChange, onLogout); break
	case "titres": pageEl = createTitresPage(data, pages.titres, onDatapageChange, onLogout); break
	case "problems": pageEl = createProblemsPage(data, onDatapageChange, onLogout); break
	default: console.error("unexpected page:", page); break
	}
	return pageEl
}

const getURLFromPageInfo = (page: DataPageID, pages: Pages): string => {
	switch (page) {
	case "participants": return "participants"; break
	case "counts": return getCountsPageURL(pages.counts); break
	case "bleeds": return `bleeds?year=${pages.bleeds.year}`; break
	case "weekly-surveys": return `weekly-surveys?year=${pages["weekly-surveys"].year}`; break
	case "titres": return getTitresPageURL(pages.titres); break
	case "problems": return "problems"; break
	default: console.error("unexpected page:", page); return getCountsPageURL(pages.counts); break
	}
}

const goToPage = (domMain: HTMLElement, page: DataPageID, pages: Pages, data: Data, onLogout: () => void) => {
	replaceChildren(domMain, createDatapage(page, pages, data, (page) => {
		const newURL = getURLFromPageInfo(page, pages)
		window.history.pushState(null, "", newURL)
		goToPage(domMain, page, pages, data, onLogout)
	}, onLogout))
}

const goToCurrentURL = (domMain: HTMLElement, data: Data, onLogout: () => void) => {

	const defCountsSettings: CountsSettings = {
		table: "records", groupsRecords: ["site"], groupsBleeds: ["year"], groupsPostinfBleeds: ["year"],
	}
	const defYear = 2022
	const defTitresSettings: TitresSettings = {
		groupsGMTs: ["year", "day"], groupsGMRs: ["year"], xFacets: ["year", "eggcell"], yFacets: ["subtype"], xAxis: "day"
	}

	const pages: Pages = {
		participants: {},
		"weekly-surveys": {year: defYear},
		bleeds: {year: defYear},
		counts: defCountsSettings,
		titres: defTitresSettings,
		problems: {},
	}

	const updatePagesAndGoToCurrent = () => {
		const urlPage = updatePageFromURL(pages)
		goToPage(domMain, urlPage, pages, data, onLogout)
	}

	// NOTE(sen) Only called once
	window.addEventListener("popstate", updatePagesAndGoToCurrent)
	updatePagesAndGoToCurrent()
}

const main = async () => {

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

	// NOTE(sen) Attempt to login from local storage
	const domMain = document.getElementById("main")!
	const onPasswordFail = () => replaceChildren(domMain, createPasswordPage(onPasswordSuccess))
	const onPasswordSuccess = (data: Data) => goToCurrentURL(domMain, data, onPasswordFail)
	let password = localStorage.getItem("password")
	if (password === null) {
		onPasswordFail()
	} else {
		replaceChildren(domMain, createLoadingPage())
		let fetchResult = await fetchData(password)
		if (!fetchResult.success) {
			onPasswordFail()
		} else {
			onPasswordSuccess(fetchResult.data)
		}
	}
}

main()

// NOTE(sen) To make this a "module"
export {}

// TODO(sen) Counts of swab results
// TODO(sen) Date filtering
// TODO(sen) Table sorting
// TODO(sen) Titre plots
