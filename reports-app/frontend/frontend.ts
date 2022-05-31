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

const arrSum = (arr: number[]) => {
	let result = 0
	for (let val of arr) {
		result += val
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
	defaultCounts: CountsType,
	filter?: (row: RowType) => boolean,
	getKey: (row: RowType, key: string) => any,
	addRow: (row: RowType, counts: CountsType) => void,
}

const summarise = <RowType, CountsType>(
	{data, groups, defaultCounts, filter, getKey, addRow}: SummariseSpec<RowType, CountsType>,
) => {
	let groupedCounts: any = {}
	if (groups.length === 0) {
		groupedCounts = { total: { ...defaultCounts } }
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
						currentGroupCount[key] = { ...defaultCounts }
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

	let summaryAos = aoaToAos(summaryAoa, namesStart.concat(Object.keys(spec.defaultCounts)))
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

//
// SECTION DOM
//

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
	"gender", "age", "aboriginal", "prior2020", "prior2021", "prior2022"] as const
const ALL_RECORD_GROUPS = ALL_RECORD_GROUPS_ as unknown as string[]
type RecordGroups = (typeof ALL_RECORD_GROUPS_)[number]

const ALL_BLEEDS_GROUPS_ = ["year", "site", "recruited", "fluArm2022", "covidArm2021",
	"gender", "age", "aboriginal", "prior2020", "prior2021", "prior2022"] as const
const ALL_BLEEDS_GROUPS = ALL_BLEEDS_GROUPS_ as unknown as string[]
type BleedsGroups = (typeof ALL_BLEEDS_GROUPS_)[number]

const ALL_POSTINF_BLEEDS_GROUPS_ = ["year", "site", "recruited", "fluArm2022", "covidArm2021",
	"gender", "age", "aboriginal", "prior2020", "prior2021", "prior2022"] as const
const ALL_POSTINF_BLEEDS_GROUPS = ALL_POSTINF_BLEEDS_GROUPS_ as unknown as string[]
type PostinfBleedsGroups = (typeof ALL_POSTINF_BLEEDS_GROUPS_)[number]

const ALL_GMT_GROUPS_ = ["year", "day", "site", "virus", "subtype", "eggcell",
	"gender", "recruited", "age_group", "prior2020", "prior2021", "prior2022"] as const
const ALL_GMT_GROUPS = ALL_GMT_GROUPS_ as unknown as string[]
type GMTGroups = (typeof ALL_GMT_GROUPS_)[number]

const ALL_GMR_GROUPS_ = ["year", "site"] as const
const ALL_GMR_GROUPS = ALL_GMR_GROUPS_ as unknown as string[]
type GMRGroups = (typeof ALL_GMR_GROUPS_)[number]

const DOWNLOAD_CSV: { [key: string]: string } = {}

const createEl = (name: string) => document.createElement(name)
const createDiv = () => createEl("div")

const setEl = (el: HTMLElement, name: string, val: string) => el.setAttribute(name, val)

const createTextline = (line: string) => {
	let div = createDiv()
	div.textContent = line
	return div
}

const addEl = (parent: HTMLElement, el: HTMLElement) => {
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
	let result = createDiv()
	result.style.whiteSpace = "nowrap"
	return result
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
	node.style.borderLeft = "1px solid var(--color-border)"
	node.style.borderRight = "1px solid var(--color-border)"
	node.style.boxSizing = "border-box"
}

const applyCellContainerStyle = (node: HTMLElement, width: number) => {
		node.style.display = "flex"
		node.style.width = width.toFixed(0) + "px"
		node.style.alignItems = "center"
		node.style.justifyContent = "center"
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
	//tableBodyContainer.style.width =
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
			if (x !== undefined && x !== null) {
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

		let headerRow = addEl(table, createTableHeaderRow(colSpec))
		addEl(table, createTableFilterRow(colSpec, (colname: string, filterVal: any) => {
			colSpec[colname].filterVal = filterVal
			aosFiltered = getAosFiltered()
			virtualizedList.setRowCount(aosFiltered.length)
		}))

		let tableBodyHeight = getTableBodyHeight(getTableHeight())
		let tableBodyContainer = addEl(table, createTableBodyContainer(getTableHeight()))
		tableBodyContainer.style.width = tableWidthPx + "px"
		titleElement.style.width = tableWidthPx + "px"

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
					addEl(rowElement, createTableCellString(spec.width, colDataFormatted))
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
	let container = createDiv()
	container.style.display = "flex"
	container.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	container.style.overflowX = "scroll"

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
		container: container, surveys: surveys, completions: completions,
		datesContainer: datesContainer,
		datesTables: { 2020: surveyDates2020.table, 2021: surveyDates2021.table, 2022: surveyDates2022.table }
	}
}

const initBleeds = (sidebarWidthPx: number) => {
	let bleeds = createDiv()
	bleeds.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	bleeds.style.overflowX = "scroll"
	let table = addDiv(bleeds)
	return { bleeds: bleeds, table: table }
}

const initParticipants = (sidebarWidthPx: number) => {
	let container = createDiv()
	container.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	container.style.overflowX = "scroll"
	let table = addDiv(container)
	return { container: container, table: table }
}

const initTitres = (sidebarWidthPx: number) => {
	let container = createDiv()
	container.style.height = `calc(100vh - ${SCROLLBAR_WIDTHS[0]}px)`
	container.style.overflowY = "scroll"
	let table = addDiv(container)
	table.style.width = `calc(100vw - ${sidebarWidthPx + SCROLLBAR_WIDTHS[1]}px)`
	table.style.overflowX = "scroll"
	let summaryTable = <HTMLElement>table.cloneNode(true)
	addEl(container, summaryTable)
	return { container: container, table: table, summaryTable: summaryTable }
}

const initCounts = (sidebarWidthPx: number) => {
	let counts = createDiv()
	counts.style.width = `calc(100vw - ${sidebarWidthPx}px)`
	counts.style.overflowX = "scroll"
	let table = addDiv(counts)
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

	let gmrGroupsSwitch = createSwitch(
		init.groupsGMRs,
		<GMRGroups[]>ALL_GMR_GROUPS,
		(groups) => {
			state.settings.titres.groupsGMRs = groups
			//window.history.pushState(null, "", getTitresPageURL(state.settings.titres))
			//updateGMRs(state)
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

	addEl(container, gmtGroupsSwitch)

	return container
}

const createCountsRecordsTable = (data: Data, groups: string[]) => {

	let withdrawalData = data.withdrawn

	let withdrawals: { [key: string]: boolean } = {}
	for (let row of withdrawalData) {
		if (row.withdrawn === 1 && row.withdrawn_reentered !== 1) {
			withdrawals[row.pid] = true
		}
	}

	let participantData = data.participants

	let groupedCounts = summarise({
		data: participantData,
		groups: groups,
		defaultCounts: { total: 0, notWithdrawn: 0, consent2022: 0, bled2022: 0 },
		filter: (row) => row.pid !== undefined && row.pid.length >= 3,
		getKey: (row, group) => {
			let key = null
			switch (group) {
				case "site": { key = row.site; } break
				case "recruited": { key = row.recruitment_year; } break
				case "gender": { key = row.gender; } break
				case "aboriginal": { key = row.atsi; } break
				case "fluArm2022": { key = row.consent_fluArm2022; } break
				case "covidArm2021": { key = row.consent_covidArm2021; } break
				case "age": { key = row.age_group; } break
				case "prior2020": { key = row.prior2020; } break
				case "prior2021": { key = row.prior2021; } break
				case "prior2022": { key = row.prior2022; } break
			}
			return key
		},
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

			if (row.latestBleedDate !== undefined) {
				if (row.latestBleedDate.startsWith("2022")) {
					counts.bled2022 += 1
				}
			}
		}
	})

	let groupedCountsFlat = flattenMap(groupedCounts, [])

	let colSpec = getColSpecFromGroups(groups)
	colSpec.total = {}
	colSpec.notWithdrawn = {}
	colSpec.consent2022 = {}
	colSpec.bled2022 = {}

	let countsAos = aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	let countsTableDesc = createTableDesc()
	addTextline(countsTableDesc, "total - total records in redcap")
	addTextline(countsTableDesc, "notWithdrawn - total records in redcap who are not withdrawn")
	addTextline(countsTableDesc, "consent2022 - total records in redcap whose latest flu conset date is from 2022")
	addTextline(countsTableDesc, "bled2022 - total records in redcap whose latest bleed date is from 2022")
	if (groups.length > 0) {
		addTextline(countsTableDesc, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}

	for (let group of groups) {
		switch (group) {
			case "recruited": { addTextline(countsTableDesc, "recruited - year the participant was recruited"); } break
			case "prior2020": { addTextline(countsTableDesc, "prior2020 - vaccination count between 2015-2019 inclusive"); } break
			case "prior2021": { addTextline(countsTableDesc, "prior2021 - vaccination count between 2016-2020 inclusive"); } break
			case "prior2022": { addTextline(countsTableDesc, "prior2022 - vaccination count between 2017-2021 inclusive"); } break
			case "fluArm2022": { addTextline(countsTableDesc, "fluArm2022 - flu arm as per 2022 consent"); } break
			case "covidArm2021": { addTextline(countsTableDesc, "covidArm2021 - covid arm as per 2021 consent"); } break
		}
	}

	let descDim = measureEl(countsTableDesc, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Record counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc)
	addEl(countsTableContainer, tableResult.table)

	return countsTableContainer
}

const createCountsBleedsTable = (data: Data, groups: string[]) => {

	let groupedCounts = summarise({
		data: data.bleed_dates,
		groups: groups,
		defaultCounts: { fluDay0: 0, fluDay7: 0, fluDay14: 0, fluDay220: 0, covDay0: 0, covDay7: 0, covDay14: 0 },
		getKey: (row, group) => {
			let key = null
			switch (group) {
				case "year": { key = row.year; } break
				case "site": { key = row.site; } break
				case "recruited": { key = row.recruitment_year; } break
				case "gender": { key = row.gender; } break
				case "aboriginal": { key = row.atsi; } break
				case "fluArm2022": { key = row.consent_fluArm2022; } break
				case "covidArm2021": { key = row.consent_covidArm2021; } break
				case "age": { key = row.age_group; } break
				case "prior2020": { key = row.prior2020; } break
				case "prior2021": { key = row.prior2021; } break
				case "prior2022": { key = row.prior2022; } break
			}
			return key
		},
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
	addTextline(countsTableDesc, "Routine bleeds that have a date in redcap")
	if (groups.length > 0) {
		addTextline(countsTableDesc, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}

	for (let group of groups) {
		switch (group) {
			case "recruited": { addTextline(countsTableDesc, "recruited - year the participant was recruited"); } break
			case "prior2020": { addTextline(countsTableDesc, "prior2020 - vaccination count between 2015-2019 inclusive"); } break
			case "prior2021": { addTextline(countsTableDesc, "prior2021 - vaccination count between 2016-2020 inclusive"); } break
			case "prior2022": { addTextline(countsTableDesc, "prior2022 - vaccination count between 2017-2021 inclusive"); } break
		}
	}

	let descDim = measureEl(countsTableDesc, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Routine bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc)
	addEl(countsTableContainer, tableResult.table)

	return countsTableContainer
}

const createCountsPostinfBleedsTable = (data: Data, groups: string[]) => {

	let groupedCounts = summarise({
		data: data.postinf_bleed_dates,
		groups: groups,
		defaultCounts: { day7: 0, day14: 0, day30: 0 },
		getKey: (row, group) => {
			let key = null
			switch (group) {
				case "year": { key = row.year; } break
				case "site": { key = row.site; } break
				case "recruited": { key = row.recruitment_year; } break
				case "gender": { key = row.gender; } break
				case "aboriginal": { key = row.atsi; } break
				case "fluArm2022": { key = row.consent_fluArm2022; } break
				case "covidArm2021": { key = row.consent_covidArm2021; } break
				case "age": { key = row.age_group; } break
				case "prior2020": { key = row.prior2020; } break
				case "prior2021": { key = row.prior2021; } break
				case "prior2022": { key = row.prior2022; } break
			}
			return key
		},
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
	addTextline(countsTableDesc, "Postnfection bleeds that have a date in redcap")
	if (groups.length > 0) {
		addTextline(countsTableDesc, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}

	for (let group of groups) {
		switch (group) {
			case "recruited": { addTextline(countsTableDesc, "recruited - year the participant was recruited"); } break
			case "prior2020": { addTextline(countsTableDesc, "prior2020 - vaccination count between 2015-2019 inclusive"); } break
			case "prior2021": { addTextline(countsTableDesc, "prior2021 - vaccination count between 2016-2020 inclusive"); } break
			case "prior2022": { addTextline(countsTableDesc, "prior2022 - vaccination count between 2017-2021 inclusive"); } break
		}
	}

	let descDim = measureEl(countsTableDesc, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	let tableResult = createTableElementFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Postinfection bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	let countsTableContainer = createDiv()
	addEl(countsTableContainer, countsTableDesc)
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
			day220: { access: "flu_day_220" },
			day0Covid: { access: "covid_day_0" },
			day7Covid: { access: "covid_day_7" },
			day14Covid: { access: "covid_day_14" },
			ari: {}
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
		},
		title: "Titres",
		getTableHeightInit: () => 500,
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
	}
	colSpec.titres = {}
	colSpec.GMT = {format: (x: any) => x.toFixed(0)}

	let tableResult = createTableElementFromAos({
		aos: titreSummary,
		colSpecInit: colSpec,
		title: "GMT",
		getTableHeightInit: () => 500,
	})

	return tableResult.table
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

const getCountSettingsFromURL = (def: CountsSettings) => {
	let urlTable = def.table
	let urlGroupsRecords = def.groupsRecords
	let urlGroupsBleeds = def.groupsBleeds
	let urlGroupsPostinfBleeds = def.groupsPostinfBleeds

	if (window.location.pathname === "/counts") {
		let params = new URLSearchParams(window.location.search)
		let needToFixAddress = false

		if (params.has("table")) {

			let tables = params.getAll("table")
			let tableIsValid = ALL_COUNTS_TABLES.includes(tables[0])
			if (tableIsValid) {
				urlTable = <CountTableID>tables[0]
			}

			if (tables.length > 1 || !tableIsValid) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

		if (params.has("record_groups")) {

			let recordGroups = params.getAll("record_groups")
			let recordGroupsFirst = recordGroups[0].split(",")
			if (allAInB(recordGroupsFirst, ALL_RECORD_GROUPS)) {
				urlGroupsRecords = <RecordGroups[]>recordGroupsFirst
			} else {
				needToFixAddress = true
			}

			if (recordGroups.length > 1) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

		if (params.has("bleeds_groups")) {

			let bleedsGroups = params.getAll("bleeds_groups")
			let bleedsGroupsFirst = bleedsGroups[0].split(",")
			if (allAInB(bleedsGroupsFirst, ALL_BLEEDS_GROUPS)) {
				urlGroupsBleeds = <BleedsGroups[]>bleedsGroupsFirst
			} else {
				needToFixAddress = true
			}

			if (bleedsGroups.length > 1) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

		if (params.has("postinf_bleeds_groups")) {

			let bleedsGroups = params.getAll("postinf_bleeds_groups")
			let bleedsGroupsFirst = bleedsGroups[0].split(",")
			if (allAInB(bleedsGroupsFirst, ALL_POSTINF_BLEEDS_GROUPS)) {
				urlGroupsPostinfBleeds = <PostinfBleedsGroups[]>bleedsGroupsFirst
			} else {
				needToFixAddress = true
			}

			if (bleedsGroups.length > 1) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

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
		let needToFixAddress = false

		if (params.has("groupsGMTs")) {

			let groups = params.getAll("groupsGMTs")
			let groupsFirst = groups[0].split(",")
			if (allAInB(groupsFirst, ALL_GMT_GROUPS)) {
				urlGroupsGMTs = <GMTGroups[]>groupsFirst
			} else {
				needToFixAddress = true
			}

			if (groups.length > 1) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

		if (params.has("groupsGMRs")) {

			let groups = params.getAll("groupsGMRs")
			let groupsFirst = groups[0].split(",")
			if (allAInB(groupsFirst, ALL_BLEEDS_GROUPS)) {
				urlGroupsGMRs = <GMRGroups[]>groupsFirst
			} else {
				needToFixAddress = true
			}

			if (groups.length > 1) {
				needToFixAddress = true
			}

		} else {
			needToFixAddress = true
		}

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
		let needToFixAddress = false

		if (params.has("year")) {
			let years = params.getAll("year")

			let yearFirst = parseInt(years[0])
			if (YEARS.includes(yearFirst)) {
				urlYear = <YearID>yearFirst
			}

			if (years.length > 1) {
				needToFixAddress = true
			}
		} else {
			needToFixAddress = true
		}

		if (needToFixAddress) {
			window.history.replaceState(null, "", `bleeds?year=${urlYear}`)
		}
	}

	return urlYear
}

const getSurveysYearFromURL = (def: YearID) => {
	let urlYear = def

	if (window.location.pathname === "/weekly-surveys") {

		let params = new URLSearchParams(window.location.search)
		let needToFixAddress = false

		if (params.has("year")) {
			let years = params.getAll("year")

			let yearFirst = parseInt(years[0])
			if (YEARS.includes(yearFirst)) {
				urlYear = <YearID>yearFirst
			}

			if (years.length > 1) {
				needToFixAddress = true
			}
		} else {
			needToFixAddress = true
		}

		if (needToFixAddress) {
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
			replaceChildren(state.elements.dataContainer, state.elements.weeklySurveys.container)
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
	state.elements.titres.summaryTable,
	createTitreGMTTable(state.filteredTitreData, state.settings.titres.groupsGMTs)
)

const updateTitres = (state: State) => {
	let titreTable = createTitreTable(state.data, (filteredData) => {
		state.filteredTitreData = filteredData
		updateGMTs(state)
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
			summaryTable: HTMLElement,
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

main()

// NOTE(sen) To make this a "module"
export {}

// TODO(sen) Counts of swab results
// TODO(sen) Date filtering
// TODO(sen) Table sorting
// TODO(sen) Titre plots
