const MISSING_STRING = "(missing)"

type Plot<X, Y> = {
	canvas: HTMLCanvasElement
	renderer: CanvasRenderingContext2D
	spec: PlotSpec<X, Y>
	scaleXToPx: (x: X, xFacets: any[]) => number
	scaleYToPx: (y: Y, yFacets: any[]) => number
	allXTicksXCoords: number[]
	allYTicksYCoords: number[]
}

type Rect = {
	l: number
	r: number
	t: number
	b: number
}

const scale = (value: number, valueMin: number, valueMax: number, scaleMin: number, scaleMax: number) => {
	let result = scaleMin
	const scaleRange = scaleMax - scaleMin
	if (scaleRange !== 0) {
		result = scaleRange / 2 + scaleMin
		const valueRange = valueMax - valueMin
		if (valueRange !== 0) {
			const value0 = value - valueMin
			const valueNorm = value0 / valueRange
			const valueScale0 = valueNorm * scaleRange
			result = valueScale0 + scaleMin
		}
	}
	return result
}

export const drawRect = (renderer: CanvasRenderingContext2D, rect: Rect, color: string) => {
	renderer.fillStyle = color
	renderer.fillRect(rect.l, rect.t, rect.r - rect.l, rect.b - rect.t)
}

export const drawLine = (
	renderer: CanvasRenderingContext2D,
	x1: number,
	y1: number,
	x2: number,
	y2: number,
	color: string,
	thiccness: number,
	dashSegments: number[]
) => {
	renderer.strokeStyle = color
	renderer.beginPath()
	renderer.moveTo(x1, y1)
	renderer.lineTo(x2, y2)
	const oldLineWidth = renderer.lineWidth
	renderer.lineWidth = thiccness

	renderer.setLineDash(dashSegments)

	renderer.stroke()

	renderer.lineWidth = oldLineWidth
	renderer.setLineDash([])
}

const drawCircle = (
	renderer: CanvasRenderingContext2D,
	centerX: number,
	centerY: number,
	radius: number,
	color: string,
	outlineColor: string
) => {
	renderer.beginPath()
	renderer.arc(centerX, centerY, radius, 0, 2 * Math.PI, false)
	renderer.fillStyle = color
	renderer.fill()
	renderer.lineWidth = 1
	renderer.strokeStyle = outlineColor
	renderer.stroke()
}

const getLineShift = (x1: number, y1: number, x2: number, y2: number, thiccness: number) => {
	const lineVec = { x: x2 - x1, y: y2 - y1 }
	const linePerpVec = { x: lineVec.y, y: lineVec.x }
	const dx = (linePerpVec.x / (linePerpVec.x + linePerpVec.y)) * thiccness
	const dy = (linePerpVec.y / (linePerpVec.x + linePerpVec.y)) * thiccness
	return { dx: dx, dy: dy }
}

export const drawDoubleLine = (
	renderer: CanvasRenderingContext2D,
	x1: number,
	y1: number,
	x2: number,
	y2: number,
	color: string,
	color2: string,
	thiccness: number,
	dashSegments: number[],
	flipShade?: boolean
) => {
	let { dx, dy } = getLineShift(x1, y1, x2, y2, thiccness)
	if (flipShade) {
		dx = -dx
		dy = -dy
	}

	drawLine(renderer, x1, y1, x2, y2, color, thiccness, dashSegments)
	drawLine(renderer, x1 + dx, y1 + dy, x2 + dx, y2 + dy, color2, thiccness, dashSegments)
}

const rectShrink = (rect: Rect, amount: number) => {
	return { l: rect.l + amount, r: rect.r - amount, t: rect.t + amount, b: rect.b - amount }
}

const drawRectOutline = (renderer: CanvasRenderingContext2D, rect: Rect, color: string, thiccness: number) => {
	const halfThicc = thiccness / 2
	drawLine(renderer, rect.l - halfThicc, rect.t, rect.r + halfThicc, rect.t, color, thiccness, [])
	drawLine(renderer, rect.r, rect.t, rect.r, rect.b, color, thiccness, [])
	drawLine(renderer, rect.l - halfThicc, rect.b, rect.r + halfThicc, rect.b, color, thiccness, [])
	drawLine(renderer, rect.l, rect.t, rect.l, rect.b, color, thiccness, [])
}

const toRadians = (val: number) => (val / 360) * 2 * Math.PI

const CANVAS_FONT_HEIGHT = 16

export const drawText = (
	renderer: CanvasRenderingContext2D,
	text: string,
	xCoord: number,
	yCoord: number,
	color: string,
	angle: number,
	baseline: CanvasTextBaseline,
	textAlign: CanvasTextAlign,
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

export type BoxplotStats = {
	min: number
	max: number
	q25: number
	median: number
	q75: number
	iqr: number
	mean: number
	meanSe: number
	meanLow: number
	meanHigh: number
}

export const getBoxplotStats = (arr: number[]): BoxplotStats | null => {
	const arrsum = (arr: number[]) => arr.reduce((a, b) => a + b, 0)
	const arrmean = (arr: number[]) => arrsum(arr) / arr.length
	const arrsd = (arr: number[]) => {
		const mu = arrmean(arr)
		const diffArr = arr.map((a) => (a - mu) ** 2)
		return Math.sqrt(arrsum(diffArr) / (arr.length - 1))
	}

	const sortedAscQuantile = (sorted: number[], q: number) => {
		const pos = (sorted.length - 1) * q
		const base = Math.floor(pos)
		const rest = pos - base
		let result = sorted[base]
		if (sorted[base + 1] !== undefined) {
			result += rest * (sorted[base + 1] - sorted[base])
		}
		return result
	}

	let result: BoxplotStats | null = null
	if (arr.length > 0) {
		const arrSorted = arr.sort((x1, x2) => x1 - x2)
		const q25 = sortedAscQuantile(arrSorted, 0.25)
		const q75 = sortedAscQuantile(arrSorted, 0.75)
		const mean = arrmean(arrSorted)
		const meanSe = arrsd(arrSorted) / Math.sqrt(arr.length)
		result = {
			min: arrSorted[0],
			max: arrSorted[arrSorted.length - 1],
			median: sortedAscQuantile(arrSorted, 0.5),
			q25: q25,
			q75: q75,
			iqr: q75 - q25,
			mean: mean,
			meanSe: meanSe,
			meanLow: mean - 1.96 * meanSe,
			meanHigh: mean + 1.96 * meanSe,
		}
	}
	return result
}

export const addBoxplot = <X, Y>(
	plot: Plot<X, Y>,
	stats: BoxplotStats,
	xCoord: number,
	totalBoxWidth: number,
	color: string,
	altColor: string,
	meanColor: string,
	lineThiccness: number
) => {
	totalBoxWidth = Math.max(totalBoxWidth, 0)
	const boxWidth = totalBoxWidth / 2
	const medianChonkiness = boxWidth / 2

	const boxLeft = xCoord - boxWidth
	const boxRight = xCoord
	const boxCenter = xCoord - boxWidth / 2

	const boxplotBody = { l: boxLeft, b: stats.q75, r: boxRight, t: stats.q25 }
	drawRectOutline(plot.renderer, boxplotBody, color, lineThiccness)
	drawRectOutline(plot.renderer, rectShrink(boxplotBody, lineThiccness), altColor, lineThiccness)

	drawDoubleLine(plot.renderer, xCoord, stats.q75, xCoord, stats.max, color, altColor, lineThiccness, [], true)

	drawDoubleLine(plot.renderer, xCoord, stats.min, xCoord, stats.q25, color, altColor, lineThiccness, [], true)

	// NOTE(sen) Median
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

	// NOTE(sen) Mean
	drawDoubleLine(
		plot.renderer,
		boxCenter,
		stats.mean + stats.meanSe * 1.96,
		boxCenter,
		stats.mean - stats.meanSe * 1.96,
		meanColor,
		altColor,
		lineThiccness,
		[]
	)
	drawCircle(plot.renderer, boxCenter, stats.mean, 5, meanColor, altColor)
}

type PlotDimType = "facet" | "plot" | "tick"

type PlotSpec<X, Y> = {
	width: number
	height: number
	widthType: PlotDimType
	heightType: PlotDimType
	scaleXData?: (x: X) => number
	scaleYData?: (y: Y) => number
	padAxis: Rect
	padData: Rect
	padFacet: number
	scaledXMin: number
	scaledXMax: number
	yMin: Y
	yMax: Y
	xTicks: X[]
	yTicks: Y[]
	xFacetSets: any[][]
	yFacetSets: any[][]
	xLabel: string
	yLabel: string
}

export const beginPlot = <X, Y>(spec: PlotSpec<X, Y>) => {
	if (spec.widthType === "tick") {
		spec.width = spec.width * spec.xTicks.length + spec.padFacet
		spec.widthType = "facet"
	}
	if (spec.heightType === "tick") {
		spec.height = spec.height * spec.yTicks.length + spec.padFacet
		spec.heightType = "facet"
	}

	if (spec.widthType === "facet") {
		spec.width =
			spec.width * spec.xFacetSets.reduce((prev, curr) => prev * curr.length, 1) +
			spec.padAxis.l +
			spec.padAxis.r +
			spec.padData.l +
			spec.padData.r
	}
	if (spec.heightType === "facet") {
		spec.height =
			spec.height * spec.yFacetSets.reduce((prev, curr) => prev * curr.length, 1) +
			spec.padAxis.t +
			spec.padAxis.b +
			spec.padData.t +
			spec.padData.b
	}

	const canvas = document.createElement("canvas")
	canvas.width = spec.width
	canvas.height = spec.height

	const renderer = canvas.getContext("2d")!

	const scaleXData = spec.scaleXData ?? ((x) => x as unknown as number)
	const scaleYData = spec.scaleYData ?? ((y) => y as unknown as number)

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
			const thisIndex = facetSet.indexOf(facet)
			panelIndex += panelsPerInc[facetIndex] * thisIndex
		}
		return panelIndex
	}

	const scaleXToPx = (val: X, xFacets: any[]) => {
		const panelIndex = getPanelIndex(xFacets, spec.xFacetSets, xPanelsMetrics.panelsPerInc)

		const facetLeft = plotMetrics.l + panelIndex * (xPanelsMetrics.facetRange + spec.padFacet)
		const facetRight = facetLeft + xPanelsMetrics.facetRange

		const result = scale(scaleXData(val), spec.scaledXMin, spec.scaledXMax, facetLeft, facetRight)
		return result
	}

	const scaleYToPx = (val: Y, yFacets: any[]) => {
		const panelIndex = getPanelIndex(yFacets, spec.yFacetSets, yPanelsMetrics.panelsPerInc)

		const facetTop = plotMetrics.t + panelIndex * (yPanelsMetrics.facetRange + spec.padFacet)
		const facetBottom = facetTop + yPanelsMetrics.facetRange

		const result = scale(scaleYData(val), scaleYData(spec.yMin), scaleYData(spec.yMax), facetBottom, facetTop)
		return result
	}

	const axisThiccness = 1
	const axisCol = "#bfbdb6"

	// NOTE(sen) Axis lines

	drawRect(
		renderer,
		{ l: spec.padAxis.l, r: spec.padAxis.l + axisThiccness, t: spec.padAxis.t, b: spec.height - spec.padAxis.b },
		axisCol
	)

	drawRect(
		renderer,
		{
			l: spec.padAxis.l,
			r: spec.width - spec.padAxis.r,
			t: spec.height - spec.padAxis.b - axisThiccness,
			b: spec.height - spec.padAxis.b,
		},
		axisCol
	)

	// NOTE(sen) Axis labels

	const axisTextCol = axisCol
	drawText(
		renderer,
		spec.xLabel,
		(plotMetrics.r - plotMetrics.l) / 2 + plotMetrics.l,
		spec.height - 3,
		axisTextCol,
		0,
		"bottom",
		"center"
	)
	drawText(
		renderer,
		spec.yLabel,
		3,
		(plotMetrics.b - plotMetrics.t) / 2 + plotMetrics.t,
		axisTextCol,
		-90,
		"top",
		"center"
	)

	// NOTE(sen) Ticks and grid

	const tickLength = 5
	const tickToText = 5

	type FacetIter = {
		facetIndices: number[]
		done: boolean
		facetSets: any[][]
	}

	const beginFacetIteration = (facetSets: any[][]): FacetIter => {
		const facetIndices = [] as number[]
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
		const facets = [] as any[]
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

	const allXTicksXCoords = []
	for (const xFacetIter = beginFacetIteration(spec.xFacetSets); !xFacetIter.done; nextFacet(xFacetIter)) {
		const xFacets = getCurrentFacetValues(xFacetIter)

		for (const xTick of spec.xTicks) {
			const xCoord = scaleXToPx(xTick, xFacets)
			allXTicksXCoords.push(xCoord)
			drawRect(
				renderer,
				{
					l: xCoord,
					r: xCoord + axisThiccness,
					t: spec.height - spec.padAxis.b,
					b: spec.height - spec.padAxis.b + tickLength,
				},
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
				"center"
			)
		}
	}

	const gridCol = axisCol + "22"
	const gridThiccness = 1

	const allYTicksYCoords = []
	for (const yFacetIter = beginFacetIteration(spec.yFacetSets); !yFacetIter.done; nextFacet(yFacetIter)) {
		const yFacets = getCurrentFacetValues(yFacetIter)

		for (const yTick of spec.yTicks) {
			const yCoord = scaleYToPx(yTick, yFacets)
			allYTicksYCoords.push(yCoord)
			drawRect(
				renderer,
				{ l: spec.padAxis.l - tickLength, r: spec.padAxis.l, t: yCoord - axisThiccness, b: yCoord },
				axisCol
			)
			drawLine(renderer, spec.padAxis.l, yCoord, spec.width - spec.padAxis.r, yCoord, gridCol, gridThiccness, [])
			drawText(
				renderer,
				`${yTick}`,
				spec.padAxis.l - tickLength - tickToText,
				yCoord,
				axisTextCol,
				0,
				"middle",
				"end"
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
						renderer,
						facetGap,
						yOffset,
						facetGap,
						spec.height - spec.padAxis.b - axisThiccness,
						facetSepColor,
						sepThiccness,
						[]
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
				drawText(
					renderer,
					`${yFacet ?? MISSING_STRING}`,
					spec.width - xOffset,
					facetCenter,
					axisTextCol,
					90,
					"top",
					"center"
				)
				drawText(
					renderer,
					`${yFacet ?? MISSING_STRING}`,
					spec.padAxis.l + xOffset,
					facetCenter,
					axisTextCol,
					-90,
					"top",
					"center"
				)
				if (setValueIndex < facetSetValues.length - 1) {
					drawLine(
						renderer,
						spec.width - xOffset,
						facetGap,
						spec.padAxis.l + xOffset,
						facetGap,
						facetSepColor,
						sepThiccness,
						[]
					)
				}
			}
			yOffset += (facetValueSpan + spec.padFacet) * facetSetValues.length
		}
		repeatLabels *= facetSetValues.length
	}

	const result: Plot<X, Y> = {
		canvas: canvas,
		renderer: renderer,
		spec: spec,
		scaleXToPx: scaleXToPx,
		scaleYToPx: scaleYToPx,
		allXTicksXCoords: allXTicksXCoords,
		allYTicksYCoords: allYTicksYCoords,
	}

	return result
}
