// Stripped down version of https://github.com/patrick-steele-idem/morphdom
const ELEMENT_NODE = 1
const TEXT_NODE = 3
const COMMENT_NODE = 8

const walkDiscardedChildNodes = (node: Node, skipKeyedNodes: boolean) => {
	if (node.nodeType === ELEMENT_NODE) {
		let curChild = node.firstChild
		while (curChild) {
			if (curChild.firstChild) {
				walkDiscardedChildNodes(curChild, skipKeyedNodes)
			}
			curChild = curChild.nextSibling
		}
	}
}

const removeNode = (node: Node, parentNode: Node, skipKeyedNodes: boolean) => {
	if (parentNode) {
		parentNode.removeChild(node)
	}
	walkDiscardedChildNodes(node, skipKeyedNodes)
}

const compareNodeNames = (fromEl: Node, toEl: Node) => {
	const fromNodeName = fromEl.nodeName
	const toNodeName = toEl.nodeName

	if (fromNodeName === toNodeName) {
		return true
	}

	const fromCodeStart = fromNodeName.charCodeAt(0)
	const toCodeStart = toNodeName.charCodeAt(0)

	// If the target element is a virtual DOM node or SVG node then we may
	// need to normalize the tag name before comparing. Normal HTML elements that are
	// in the "http://www.w3.org/1999/xhtml"
	// are converted to upper case
	if (fromCodeStart <= 90 && toCodeStart >= 97) {
		// from is upper and to is lower
		return fromNodeName === toNodeName.toUpperCase()
	} else if (toCodeStart <= 90 && fromCodeStart >= 97) {
		// to is upper and from is lower
		return toNodeName === fromNodeName.toUpperCase()
	} else {
		return false
	}
}

const morphEl = (fromEl: Node, toEl: Node) => {
	let curToNodeChild = toEl.firstChild
	let curFromNodeChild = fromEl.firstChild

	let fromNextSibling = null
	let toNextSibling

	// walk the children
	outer: while (curToNodeChild) {
		toNextSibling = curToNodeChild.nextSibling

		// walk the fromNode children all the way through
		while (curFromNodeChild !== null) {
			fromNextSibling = curFromNodeChild.nextSibling

			if (curToNodeChild.isSameNode && curToNodeChild.isSameNode(curFromNodeChild)) {
				curToNodeChild = toNextSibling
				curFromNodeChild = fromNextSibling
				continue outer
			}

			const curFromNodeType = curFromNodeChild.nodeType

			// this means if the curFromNodeChild doesnt have a match with the curToNodeChild
			let isCompatible = undefined

			if (curFromNodeType === curToNodeChild.nodeType) {
				if (curFromNodeType === ELEMENT_NODE && curFromNodeChild) {
					isCompatible = isCompatible !== false && compareNodeNames(curFromNodeChild, curToNodeChild)
					if (isCompatible) {
						// We found compatible DOM elements so transform
						// the current "from" node to match the current
						// target DOM node.
						// MORPH
						morphEl(curFromNodeChild, curToNodeChild)
					}
				} else if (curFromNodeType === TEXT_NODE || curFromNodeType == COMMENT_NODE) {
					// Both nodes being compared are Text or Comment nodes
					isCompatible = true
					// Simply update nodeValue on the original node to
					// change the text value
					if (curFromNodeChild.nodeValue !== curToNodeChild.nodeValue) {
						curFromNodeChild.nodeValue = curToNodeChild.nodeValue
					}
				}
			}

			if (isCompatible) {
				// Advance both the "to" child and the "from" child since we found a match
				// Nothing else to do as we already recursively called morphChildren above
				curToNodeChild = toNextSibling
				curFromNodeChild = fromNextSibling
				continue outer
			}

			if (curFromNodeChild !== null) {
				removeNode(curFromNodeChild, fromEl, true /* skip keyed nodes */)
			}

			curFromNodeChild = fromNextSibling
		} // while(curFromNodeChild)

		// If we got this far then we did not find a candidate match for
		// our "to node" and we exhausted all of the children "from"
		// nodes. Therefore, we will just append the current "to" node
		// to the end
		fromEl.appendChild(curToNodeChild)

		curToNodeChild = toNextSibling
		curFromNodeChild = fromNextSibling
	}

	// We have processed all of the "to nodes". If curFromNodeChild is
	// non-null then we still have some from nodes left over that need
	// to be removed
	while (curFromNodeChild) {
		const fromNextSibling = curFromNodeChild.nextSibling
		removeNode(curFromNodeChild, fromEl, true /* skip keyed nodes */)
		curFromNodeChild = fromNextSibling
	}
}

const morphdom = (fromNode: HTMLElement, toNode: HTMLElement) => {
	if (fromNode !== toNode) {
		if (!(toNode.isSameNode && toNode.isSameNode(fromNode))) {
			morphEl(fromNode, toNode)
		}
	}
}

// Based on https://github.com/clauderic/virtualized-list

type SizeAndPosition = {
	itemCount: number
	itemSize: number
	// Cache of size and position data for items, mapped by item index.
	itemSizeAndPositionData: { [key in number]: { offset: number; size: number } }
	// Measurements for items up to this index can be trusted; items afterward should be estimated.
	lastMeasuredIndex: number
}

const createSizeAndPosition = (itemCount: number, itemSize: number) => {
	const result: SizeAndPosition = {
		itemCount: itemCount,
		itemSize: itemSize,
		itemSizeAndPositionData: [],
		lastMeasuredIndex: -1,
	}
	return result
}

const getSizeAndPositionOfLastMeasuredItem = (sp: SizeAndPosition) => {
	return sp.lastMeasuredIndex >= 0 ? sp.itemSizeAndPositionData[sp.lastMeasuredIndex] : { offset: 0, size: 0 }
}

const getTotalSize = (sp: SizeAndPosition) => {
	const lastMeasuredSizeAndPosition = getSizeAndPositionOfLastMeasuredItem(sp)
	return (
		lastMeasuredSizeAndPosition.offset +
		lastMeasuredSizeAndPosition.size +
		(sp.itemCount - sp.lastMeasuredIndex - 1) * sp.itemSize
	)
}

const getSizeAndPositionForIndex = (sp: SizeAndPosition, index: number) => {
	index = Math.max(Math.min(index, sp.itemCount - 1), 0)

	if (index > sp.lastMeasuredIndex) {
		const lastMeasuredSizeAndPosition = getSizeAndPositionOfLastMeasuredItem(sp)
		let offset = lastMeasuredSizeAndPosition.offset + lastMeasuredSizeAndPosition.size

		for (let i = sp.lastMeasuredIndex + 1; i <= index; i++) {
			const size = sp.itemSize
			sp.itemSizeAndPositionData[i] = {
				offset,
				size,
			}
			offset += size
		}

		sp.lastMeasuredIndex = index
	}

	return sp.itemSizeAndPositionData[index]
}

/**
 * Searches for the item (index) nearest the specified offset.
 *
 * If no exact match is found the next lowest item index will be returned.
 * This allows partially visible items (with offsets just before/above the fold) to be visible.
 */
const findNearestItem = (sp: SizeAndPosition, offset: number) => {
	// Our search algorithms find the nearest match at or below the specified offset.
	// So make sure the offset is at least 0 or no match will be found.
	offset = Math.max(0, offset)

	const lastMeasuredSizeAndPosition = getSizeAndPositionOfLastMeasuredItem(sp)
	const lastMeasuredIndex = Math.max(0, sp.lastMeasuredIndex)

	if (lastMeasuredSizeAndPosition.offset >= offset) {
		// If we've already measured items within this range just use a binary search as it's faster.
		return binarySearch(sp, 0, lastMeasuredIndex, offset)
	} else {
		// If we haven't yet measured this high, fallback to an exponential search with an inner binary search.
		// The exponential search avoids pre-computing sizes for the full set of items as a binary search would.
		// The overall complexity for this approach is O(log n).
		return exponentialSearch(sp, lastMeasuredIndex, offset)
	}
}

const getVisibleRange = (sp: SizeAndPosition, containerSize: number, offset: number) => {
	const totalSize = getTotalSize(sp)

	if (totalSize === 0) {
		return { start: 0, stop: 0 }
	}

	const maxOffset = offset + containerSize
	const start = findNearestItem(sp, offset)
	let stop = start

	const datum = getSizeAndPositionForIndex(sp, start)
	offset = datum.offset + datum.size

	while (offset < maxOffset && stop < sp.itemCount - 1) {
		stop++
		offset += getSizeAndPositionForIndex(sp, stop).size
	}

	return {
		start,
		stop,
	}
}

const binarySearch = (sp: SizeAndPosition, low: number, high: number, offset: number) => {
	let middle
	let currentOffset

	while (low <= high) {
		middle = low + Math.floor((high - low) / 2)
		currentOffset = getSizeAndPositionForIndex(sp, middle).offset

		if (currentOffset === offset) {
			return middle
		} else if (currentOffset < offset) {
			low = middle + 1
		} else if (currentOffset > offset) {
			high = middle - 1
		}
	}

	if (low > 0) {
		return low - 1
	}

	return low
}

const exponentialSearch = (sp: SizeAndPosition, index: number, offset: number) => {
	let interval = 1
	while (index < sp.itemCount && getSizeAndPositionForIndex(sp, index).offset < offset) {
		index += interval
		interval *= 2
	}
	return binarySearch(sp, Math.floor(index / 2), Math.min(index, sp.itemCount - 1), offset)
}

export type VirtualizedListOptions = {
	height: number
	rowCount: number
	renderRow: (rowIndex: number) => HTMLElement
	rowHeight: number
}

export type VirtualizedList = {
	container: HTMLElement
	inner: HTMLElement
	content: HTMLElement
	height: number
	offset: number
	sizeAndPosition: SizeAndPosition
	renderRow: (rowIndex: number) => HTMLElement
}

export const createVirtualizedList = (container: HTMLElement, options: VirtualizedListOptions) => {
	const vl: VirtualizedList = {
		container: container,
		height: options.height,
		offset: 0,
		sizeAndPosition: createSizeAndPosition(options.rowCount, options.rowHeight),
		inner: document.createElement("div"),
		content: document.createElement("div"),
		renderRow: options.renderRow,
	}

	vl.inner.setAttribute(
		"style",
		"position:relative; overflow:hidden; width:100%; min-height:100%; will-change: transform;"
	)
	vl.content.setAttribute("style", "position:absolute; top:0; left:0; height:100%; width:100%; overflow:visible;")
	vl.inner.appendChild(vl.content)
	container.appendChild(vl.inner)

	container.scrollTop = vl.offset
	container.addEventListener("scroll", () => {
		vl.offset = vl.container.scrollTop
		requestAnimationFrame(() => vlRender(vl))
	})

	requestAnimationFrame(() => vlRender(vl))
	return vl
}

export const setHeight = (vl: VirtualizedList, height: number) => {
	vl.height = height
	requestAnimationFrame(() => vlRender(vl))
}

export const setRowCount = (vl: VirtualizedList, count: number) => {
	vl.sizeAndPosition = createSizeAndPosition(count, vl.sizeAndPosition.itemSize)
	vlRender(vl)
}

const vlRender = (vl: VirtualizedList) => {
	const { start, stop } = getVisibleRange(vl.sizeAndPosition, vl.height, vl.offset)
	const fragment = document.createElement("div")
	for (let index = start; index <= stop; index++) {
		fragment.appendChild(vl.renderRow(index))
	}
	vl.inner.style.height = `${getTotalSize(vl.sizeAndPosition)}px`
	vl.content.style.top = `${getSizeAndPositionForIndex(vl.sizeAndPosition, start).offset}px`
	morphdom(vl.content, fragment)
}

const MISSING_STRING = "(missing)"
const TABLE_ROW_HEIGHT_PX = 30
const DOWNLOAD_CSV: { [key: string]: string } = {}

const getScrollbarWidths = () => {
	const outer = document.createElement("div")
	outer.style.visibility = "hidden"
	outer.style.overflowY = "scroll"
	document.body.appendChild(outer)

	const inner = document.createElement("div")
	outer.appendChild(inner)

	const scrollbarWidthV = outer.offsetWidth - inner.offsetWidth
	outer.removeChild(inner)

	outer.style.overflowY = "hidden"
	outer.style.overflowX = "scroll"

	outer.appendChild(inner)
	const scrollbarWidthH = outer.offsetHeight - inner.offsetHeight

	outer.parentNode!.removeChild(outer)
	return [scrollbarWidthH, scrollbarWidthV]
}

const SCROLLBAR_WIDTHS = getScrollbarWidths()

const createEl = document.createElement.bind(document)
const createDiv = () => createEl("div")
const addEl = <T1 extends HTMLElement, T2 extends HTMLElement>(parent: T1, child: T2) => {
	parent.appendChild(child)
	return child
}
const addDiv = (parent: HTMLElement) => addEl(parent, createDiv())

const createTableFilterRow = <T>(colSpec: { [key: string]: TableColSpecFinal<T> }, onInput: any) => {
	const filterRow = createDiv()
	applyTableHeaderRowStyle(filterRow)

	let rowWidth = 0
	let colnameIndex = 0
	for (const colname of Object.keys(colSpec)) {
		const colWidthPx = colSpec[colname].width
		rowWidth += colWidthPx
		const cellContainer = addDiv(filterRow)
		applyCellContainerStyle(cellContainer, colWidthPx)
		cellContainer.style.position = "relative"

		const questionMarkWidth = 20

		const cell = <HTMLInputElement>addEl(cellContainer, createEl("input"))
		cell.type = "text"
		cell.autocomplete = "off"
		cell.placeholder = "Filter..."
		cell.style.width = colWidthPx - questionMarkWidth + "px"
		cell.style.boxSizing = "border-box"
		cell.addEventListener("input", (event) => {
			onInput(colname, (<HTMLTextAreaElement>event.target).value)
		})

		const questionMark = addDiv(cellContainer)
		questionMark.style.padding = "2px"
		questionMark.style.width = questionMarkWidth + "px"
		questionMark.style.textAlign = "center"
		questionMark.style.cursor = "pointer"
		questionMark.textContent = "?"

		const helpText =
			"Case-sensitive. Supports regular expressions (e.g. ^male). For numbers, you can type >x and <x (e.g. >40)"
		const helpEl = createDiv()
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
				cellContainer.removeChild(helpEl)
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

const applyTableHeaderRowStyle = (node: HTMLElement) => {
	node.style.display = "flex"
	node.style.height = TABLE_ROW_HEIGHT_PX + "px"
	node.style.backgroundColor = "var(--color-background2)"
	node.style.boxSizing = "border-box"
}

const applyCellContainerStyle = (node: HTMLElement, width: number) => {
	node.style.display = "flex"
	node.style.width = width.toFixed(0) + "px"
	node.style.alignItems = "center"
	node.style.justifyContent = "center"
	node.style.whiteSpace = "nowrap"
}

const createTableHeaderRow = <T>(colSpec: { [key: string]: TableColSpecFinal<T> }) => {
	const headerRow = createDiv()
	applyTableHeaderRowStyle(headerRow)

	let rowWidth = 0
	for (const colname of Object.keys(colSpec)) {
		const colWidthPx = colSpec[colname].width
		rowWidth += colWidthPx
		const cell = addDiv(headerRow)
		applyCellContainerStyle(cell, colWidthPx)
		cell.textContent = colname
	}

	headerRow.style.width = rowWidth + "px"
	return headerRow
}

const createTableCell = (widthPx: number) => {
	const cellElement = createEl("td")
	cellElement.style.width = widthPx + "px"
	cellElement.style.textAlign = "center"
	cellElement.style.verticalAlign = "middle"
	cellElement.style.whiteSpace = "nowrap"
	return cellElement
}

const createTableCellString = (widthPx: number, string: string) => {
	const cellElement = createTableCell(widthPx)
	cellElement.textContent = string
	if (string === MISSING_STRING) {
		cellElement.style.color = "var(--color-text-muted)"
	}
	return cellElement
}

const createTableTitle = (title: string, downloadable: boolean) => {
	const titleElement = createDiv()
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

		titleElement.addEventListener("click", () => {
			const csv = DOWNLOAD_CSV[title]
			if (csv) {
				const hidden = createEl("a")
				hidden.href = "data:text/csv;charset=utf-8," + encodeURIComponent(csv)
				hidden.target = "_blank"
				hidden.download = title + ".csv"
				hidden.click()
			} else {
				console.error(`table '${title}' does not have a csv to download`)
			}
		})
	}

	return titleElement
}

const getTableBodyHeight = (tableHeight: number) => {
	const result = tableHeight - TABLE_ROW_HEIGHT_PX * 3
	return result
}

const createTableBodyContainer = (tableHeight: number) => {
	const tableBodyContainer = createDiv()
	tableBodyContainer.style.overflowY = "scroll"
	tableBodyContainer.style.maxHeight = getTableBodyHeight(tableHeight) + "px"
	tableBodyContainer.style.boxSizing = "border-box"
	return tableBodyContainer
}

const getTableRowBackgroundColor = (rowIndex: number) => {
	let result = "var(--color-background)"
	if (rowIndex % 2 == 1) {
		result = "var(--color-background2)"
	}
	return result
}

const createTableDataRow = (rowIndex: number) => {
	const rowElement = createEl("tr")
	rowElement.style.height = TABLE_ROW_HEIGHT_PX + "px"
	rowElement.style.backgroundColor = getTableRowBackgroundColor(rowIndex)
	return rowElement
}

type TableColSpecFinal<RowType> = {
	access: (row: RowType) => any
	format: (val: any) => string
	width: number
	filter: (row: RowType, val: string) => boolean
	filterVal: string
	filterValProcess: (val: string) => string
}

export type TableColSpec<RowType> = {
	access?: ((row: RowType) => any) | string
	format?: (val: any) => string
	width?: number
	filter?: (row: RowType, val: any) => boolean
	filterValProcess?: (val: string) => string
}

export const createTableFromAos = <RowType extends { [key: string]: any }>({
	aos,
	colSpecInit,
	defaults,
	title,
	forRow,
	getTableHeightInit,
	onFilterChange,
}: {
	aos: RowType[]
	colSpecInit: { [key: string]: TableColSpec<RowType> }
	title: string
	defaults?: TableColSpec<RowType>
	forRow?: (row: RowType) => void
	getTableHeightInit?: () => number
	onFilterChange?: (filteredData: RowType[]) => void
}) => {
	const getTableHeight = getTableHeightInit ?? (() => window.innerHeight - SCROLLBAR_WIDTHS[0])

	const table = createDiv()
	table.style.maxWidth = "100%"
	addEl(table, createTableTitle(title, true))
	DOWNLOAD_CSV[title] = ""

	const colnames = Object.keys(colSpecInit)
	DOWNLOAD_CSV[title] += colnames.map((x) => `"${x}"`).join(",") + "\n"

	// NOTE(sen) Fill in missing spec entries
	const colSpec: { [key: string]: TableColSpecFinal<RowType> } = {}
	for (const colname of colnames) {
		const specInit = colSpecInit[colname]

		let accessInit = specInit.access ?? defaults?.access ?? colname
		if (typeof accessInit === "string" || accessInit instanceof String) {
			const colname = <string>accessInit
			accessInit = (rowData) => rowData[colname]
		}

		const access = <(row: RowType) => any>accessInit
		const format = (x: any) => {
			let result = MISSING_STRING
			if (x !== undefined && x !== null && x !== "undefined") {
				const formatTest = specInit.format ?? defaults?.format
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
			filter:
				specInit.filter ??
				defaults?.filter ??
				((row, val) => {
					const data = access(row)
					const formattedData = format(data)
					let passed = true

					if ((val.startsWith(">") || val.startsWith("<")) && typeof data === "number") {
						const valNumber = parseFloat(val.slice(1))
						if (!isNaN(valNumber)) {
							switch (val[0]) {
								case ">":
									passed = data >= valNumber
									break
								case "<":
									passed = data <= valNumber
									break
							}
						}
					} else {
						try {
							const re = new RegExp(val)
							const reResult = formattedData.search(re)
							passed = reResult !== -1
						} catch (_error) {
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
	for (const colname of colnames) {
		tableWidthPx += colSpec[colname].width
	}

	if (aos.length > 0) {
		const hscrollContainer = addDiv(table)
		hscrollContainer.style.overflowX = "scroll"
		hscrollContainer.style.boxSizing = "border-box"
		hscrollContainer.style.borderLeft = "1px solid var(--color-border)"
		hscrollContainer.style.borderRight = "1px solid var(--color-border)"

		addEl(hscrollContainer, createTableHeaderRow(colSpec))
		addEl(
			hscrollContainer,
			createTableFilterRow(colSpec, (colname: string, filterVal: any) => {
				colSpec[colname].filterVal = colSpec[colname].filterValProcess(filterVal)
				aosFiltered = getAosFiltered()
				setRowCount(virtualizedList, aosFiltered.length)
			})
		)

		let tableBodyHeight = getTableBodyHeight(getTableHeight())
		const tableBodyContainer = addEl(hscrollContainer, createTableBodyContainer(getTableHeight()))
		tableBodyContainer.style.width = tableWidthPx + "px"

		const getAosFiltered = () => {
			const aosFiltered: RowType[] = []
			for (let rowIndex = 0; rowIndex < aos.length; rowIndex += 1) {
				const rowData = aos[rowIndex]

				let passedColFilters = true
				for (const otherColname of colnames) {
					const spec = colSpec[otherColname]
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

		const virtualizedList = createVirtualizedList(tableBodyContainer, {
			height: tableBodyHeight,
			rowCount: aosFiltered.length,
			renderRow: (rowIndex: number) => {
				const rowData = aosFiltered[rowIndex]
				const rowElement = createTableDataRow(rowIndex)

				for (const colname of colnames) {
					const spec = colSpec[colname]
					const colData = spec.access(rowData)
					const colDataFormatted = spec.format(colData)
					const width = spec.width - SCROLLBAR_WIDTHS[1] / colnames.length
					addEl(rowElement, createTableCellString(width, colDataFormatted))
				}

				return rowElement
			},
			rowHeight: TABLE_ROW_HEIGHT_PX,
		})

		const regenBody = () => {
			const newTableBodyHeight = getTableBodyHeight(getTableHeight())
			if (newTableBodyHeight != tableBodyHeight) {
				tableBodyHeight = newTableBodyHeight
				tableBodyContainer.style.maxHeight = newTableBodyHeight + "px"
				setHeight(virtualizedList, newTableBodyHeight)
			}
		}

		for (let rowIndex = 0; rowIndex < aos.length; rowIndex += 1) {
			const rowData = aos[rowIndex]

			for (let colnameIndex = 0; colnameIndex < colnames.length; colnameIndex++) {
				const colname = colnames[colnameIndex]
				const spec = colSpec[colname]
				const colData = spec.access(rowData)
				const colDataFormatted = spec.format(colData)
				DOWNLOAD_CSV[title] += '"' + colDataFormatted + '"'
				if (colnameIndex < colnames.length - 1) {
					DOWNLOAD_CSV[title] += ","
				}
			}

			DOWNLOAD_CSV[title] += "\n"
			forRow?.(rowData)
		}

		globalThis.window.addEventListener("resize", regenBody)
	}

	return table
}
