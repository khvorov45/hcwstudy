import * as DOM from "./dom.ts"
import * as Arr from "./array.ts"
import * as Table from "./table.ts"
import * as Plot from "./plot.ts"
import * as Rand from "./rand.ts"
import * as Switch from "./switch.ts"

//
// SECTION Utilities
//

const addDays = (date: Date, days: number) => {
	const dateCopy = new Date(date)
	dateCopy.setDate(date.getDate() + days)
	return dateCopy
}

const allAInB = <T>(arrA: T[], arrB: T[]) => {
	let result = true
	for (const entryA of arrA) {
		if (!arrB.includes(entryA)) {
			result = false
			break
		}
	}
	return result
}

const isString = (val: any) => typeof val === "string" || val instanceof String
const isNumber = (val: any) => typeof val === "number"

const formatDate = (date: Date) => date.toISOString().slice(0, 10)

const getColSpecFromGroups = (groups: string[]) => {
	const colSpec: any = {}
	if (groups.length === 0) {
		colSpec.Total = {}
	}
	for (const group of groups) {
		colSpec[group] = {}
	}
	return colSpec
}

//
// SECTION DOM
//

const SIDEBAR_WIDTH_PX = 180

const YEARS_ = [2020, 2021, 2022] as const
const YEARS = YEARS_ as unknown as number[]
type YearID = typeof YEARS_[number]

const ALL_DATAPAGE_IDS_ = ["participants", "weekly-surveys", "bleeds", "counts", "titres", "problems"] as const
const ALL_DATAPAGE_IDS = ALL_DATAPAGE_IDS_ as unknown as string[]
type DataPageID = typeof ALL_DATAPAGE_IDS_[number]

const ALL_COUNTS_TABLES_ = ["records", "routine-bleeds", "postinfection-bleeds"] as const
const ALL_COUNTS_TABLES = ALL_COUNTS_TABLES_ as unknown as string[]
type CountTableID = typeof ALL_COUNTS_TABLES_[number]

const ALL_RECORD_GROUPS_ = [
	"site",
	"recruited",
	"fluArm2022",
	"covidArm2021",
	"gender",
	"age",
	"aboriginal",
	"prior2020",
	"prior2021",
	"prior2022",
	"vax2020",
	"vax2021",
	"vax2022",
] as const
const ALL_RECORD_GROUPS = ALL_RECORD_GROUPS_ as unknown as string[]
type RecordGroups = typeof ALL_RECORD_GROUPS_[number]

const ALL_BLEEDS_GROUPS_ = [
	"year",
	"site",
	"recruited",
	"fluArm2022",
	"covidArm2021",
	"gender",
	"age",
	"aboriginal",
	"prior2020",
	"prior2021",
	"prior2022",
	"vax2020",
	"vax2021",
	"vax2022",
	"flupos2020",
	"flupos2021",
	"flupos2022",
] as const
const ALL_BLEEDS_GROUPS = ALL_BLEEDS_GROUPS_ as unknown as string[]
type BleedsGroups = typeof ALL_BLEEDS_GROUPS_[number]

const ALL_POSTINF_BLEEDS_GROUPS_ = ALL_BLEEDS_GROUPS_
const ALL_POSTINF_BLEEDS_GROUPS = ALL_POSTINF_BLEEDS_GROUPS_ as unknown as string[]
type PostinfBleedsGroups = typeof ALL_POSTINF_BLEEDS_GROUPS_[number]

const ALL_GMT_GROUPS_ = [
	"year",
	"day",
	"site",
	"vax_inf",
	"virus",
	"subtype",
	"eggcell",
	"gender",
	"recruited",
	"age_group",
	"prior2020",
	"prior2021",
	"prior2022",
	"vax2020",
	"vax2021",
	"vax2022",
	"flupos2020",
	"flupos2021",
	"flupos2022",
] as const
const ALL_GMT_GROUPS = ALL_GMT_GROUPS_ as unknown as string[]
type GMTGroups = typeof ALL_GMT_GROUPS_[number]

const ALL_GMR_GROUPS_ = [
	"year",
	"site",
	"vax_inf",
	"virus",
	"subtype",
	"eggcell",
	"gender",
	"recruited",
	"age_group",
	"prior2020",
	"prior2021",
	"prior2022",
	"vax2020",
	"vax2021",
	"vax2022",
	"flupos2020",
	"flupos2021",
	"flupos2022",
] as const
const ALL_GMR_GROUPS = ALL_GMR_GROUPS_ as unknown as string[]
type GMRGroups = typeof ALL_GMR_GROUPS_[number]

const ALL_TITRE_FACETS_ = ALL_GMT_GROUPS_
const ALL_TITRE_FACETS = ALL_TITRE_FACETS_ as unknown as string[]
type TitreFacets = typeof ALL_TITRE_FACETS_[number]

const ALL_SUBTYPES_ = ["H1", "H3", "BVic", "BYam"] as const
type Subtypes = typeof ALL_SUBTYPES_[number]

const ALL_EGGCELL_ = ["egg", "cell"] as const
type EggCell = typeof ALL_EGGCELL_[number]

const TITRES_HELP_HEIGHT = 100

const getParticipantsKey = (row: any, group: string) => {
	let key = row[group]
	switch (group) {
		case "recruited":
			key = row.recruitment_year
			break
		case "aboriginal":
			key = row.atsi
			break
		case "fluArm2022":
			key = row.consent_fluArm2022
			break
		case "covidArm2021":
			key = row.consent_covidArm2021
			break
		case "age":
			key = row.age_group
			break
		case "vax2020":
			key = row.study_year_vac_2020
			break
		case "vax2021":
			key = row.study_year_vac_2021
			break
		case "vax2022":
			key = row.study_year_vac_2022
			break
	}
	return key
}

const addGroupsExplanatoryNotes = (node: HTMLElement, groups: string[]) => {
	for (const group of groups) {
		switch (group) {
			case "recruited":
				DOM.addDivWithText(node, "recruited - year the participant was recruited")
				break
			case "prior2020":
				DOM.addDivWithText(node, "prior2020 - vaccination count between 2015-2019 inclusive")
				break
			case "prior2021":
				DOM.addDivWithText(node, "prior2021 - vaccination count between 2016-2020 inclusive")
				break
			case "prior2022":
				DOM.addDivWithText(node, "prior2022 - vaccination count between 2017-2021 inclusive")
				break
			case "vax2020":
				DOM.addDivWithText(
					node,
					"vax2020 - vaccinated in 2020 (vaccination recorded or day14 bleed taken or day14 titre present)"
				)
				break
			case "vax2021":
				DOM.addDivWithText(
					node,
					"vax2021 - vaccinated in 2021 (vaccination recorded or day14 bleed taken or day14 titre present)"
				)
				break
			case "vax2022":
				DOM.addDivWithText(
					node,
					"vax2022 - vaccinated in 2022 (vaccination recorded or day14 bleed taken or day14 titre present)"
				)
				break
			case "fluArm2022":
				DOM.addDivWithText(node, "fluArm2022 - flu arm as per 2022 consent")
				break
			case "covidArm2021":
				DOM.addDivWithText(node, "covidArm2021 - covid arm as per 2021 consent")
				break
			case "flupos2020":
				DOM.addDivWithText(node, "flupos2020 - total records in redcap that have a flu positive swab in 2020")
				break
			case "flupos2021":
				DOM.addDivWithText(node, "flupos2021 - total records in redcap that have a flu positive swab in 2021")
				break
			case "flupos2022":
				DOM.addDivWithText(node, "flupos2022 - total records in redcap that have a flu positive swab in 2022")
				break
		}
	}
}

const addCountsExplanatoryNotes = (node: HTMLElement, groups: string[]) => {
	if (groups.length > 0) {
		DOM.addDivWithText(node, "all counts apply to the subset defined by (" + groups.join(", ") + ")")
	}
	addGroupsExplanatoryNotes(node, groups)
}

const createTableDesc = () => {
	const container = DOM.createDiv()
	container.style.overflowX = "scroll"
	container.style.maxWidth = "100%"
	const desc = DOM.addDiv(container)
	desc.style.whiteSpace = "nowrap"
	return { container: container, desc: desc }
}

const createCountsRecordsTable = (data: Data, groups: RecordGroups[]) => {
	const withdrawalData = data.withdrawn

	const withdrawals: { [key: string]: boolean } = {}
	for (const row of withdrawalData) {
		if (row.withdrawn === 1 && row.withdrawn_reentered !== 1) {
			withdrawals[row.pid] = true
		}
	}

	const groupedCounts = Arr.summarise({
		data: data.participants,
		groups: groups,
		defaultCounts: {
			total: 0,
			notWithdrawn: 0,
			consent2022: 0,
			bled2020: 0,
			bled2021: 0,
			bled2022: 0,
			flupos2020: 0,
			flupos2021: 0,
			flupos2022: 0,
		},
		filter: (row) => row.pid !== undefined && row.pid.length >= 3,
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			const withdrawn = withdrawals[row.pid] === true
			const notWithdrawn = !withdrawn
			counts.total += 1
			if (notWithdrawn) {
				counts.notWithdrawn += 1
			}

			const consentDate = row.date_fluArm2022
			if (consentDate !== undefined) {
				if (consentDate.startsWith("2022")) {
					counts.consent2022 += 1
				}
			}

			if (row.bled2020 === 1) {
				counts.bled2020 += 1
			}
			if (row.bled2021 === 1) {
				counts.bled2021 += 1
			}
			if (row.bled2022 === 1) {
				counts.bled2022 += 1
			}
			if (row.flupos2020 === 1) {
				counts.flupos2020 += 1
			}
			if (row.flupos2021 === 1) {
				counts.flupos2021 += 1
			}
			if (row.flupos2022 === 1) {
				counts.flupos2022 += 1
			}
		},
	})

	const groupedCountsFlat = Arr.flattenMap(groupedCounts, [])

	const colSpec = getColSpecFromGroups(groups)
	colSpec.total = {}
	colSpec.notWithdrawn = {}
	colSpec.consent2022 = {}
	colSpec.bled2020 = {}
	colSpec.bled2021 = {}
	colSpec.bled2022 = {}
	colSpec.flupos2020 = {}
	colSpec.flupos2021 = {}
	colSpec.flupos2022 = {}

	const countsAos = Arr.aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	const countsTableDesc = createTableDesc()
	DOM.addDivWithText(countsTableDesc.desc, "total - total records in redcap")
	DOM.addDivWithText(countsTableDesc.desc, "notWithdrawn - total records in redcap who are not withdrawn")
	DOM.addDivWithText(
		countsTableDesc.desc,
		"consent2022 - total records in redcap whose latest flu conset date is from 2022"
	)
	DOM.addDivWithText(countsTableDesc.desc, "bled2020 - total records in redcap that have any bleed date in 2020")
	DOM.addDivWithText(countsTableDesc.desc, "bled2021 - total records in redcap that have any bleed date in 2021")
	DOM.addDivWithText(countsTableDesc.desc, "bled2022 - total records in redcap that have any bleed date in 2022")
	DOM.addDivWithText(
		countsTableDesc.desc,
		"flupos2020 - total records in redcap that have a flu positive swab in 2020"
	)
	DOM.addDivWithText(
		countsTableDesc.desc,
		"flupos2021 - total records in redcap that have a flu positive swab in 2021"
	)
	DOM.addDivWithText(
		countsTableDesc.desc,
		"flupos2022 - total records in redcap that have a flu positive swab in 2022"
	)
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	const descDim = DOM.measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	const tableEl = Table.createTableFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Record counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - DOM.SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	const countsTableContainer = DOM.createDiv()
	DOM.addEl(countsTableContainer, countsTableDesc.container)
	DOM.addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createCountsBleedsTable = (data: Data, groups: BleedsGroups[]) => {
	const groupedCounts = Arr.summarise({
		data: data.bleed_dates,
		groups: groups,
		defaultCounts: { fluDay0: 0, fluDay7: 0, fluDay14: 0, fluDay220: 0, covDay0: 0, covDay7: 0, covDay14: 0 },
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			const isPresent = (val: any) => val !== null && val !== undefined && val !== ""
			if (isPresent(row.flu_day_0)) {
				counts.fluDay0 += 1
			}
			if (isPresent(row.flu_day_7)) {
				counts.fluDay7 += 1
			}
			if (isPresent(row.flu_day_14)) {
				counts.fluDay14 += 1
			}
			if (isPresent(row.flu_day_220)) {
				counts.fluDay220 += 1
			}
			if (isPresent(row.covid_day_0)) {
				counts.covDay0 += 1
			}
			if (isPresent(row.covid_day_7)) {
				counts.covDay7 += 1
			}
			if (isPresent(row.covid_day_14)) {
				counts.covDay14 += 1
			}
		},
	})

	const groupedCountsFlat = Arr.flattenMap(groupedCounts, [])

	const colSpec = getColSpecFromGroups(groups)
	colSpec.fluDay0 = {}
	colSpec.fluDay7 = {}
	colSpec.fluDay14 = {}
	colSpec.fluDay220 = {}
	colSpec.covDay0 = {}
	colSpec.covDay7 = {}
	colSpec.covDay14 = {}

	const countsAos = Arr.aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	const countsTableDesc = createTableDesc()
	DOM.addDivWithText(countsTableDesc.desc, "Routine bleeds that have a date in redcap")
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	const descDim = DOM.measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	const tableEl = Table.createTableFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Routine bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - DOM.SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	const countsTableContainer = DOM.createDiv()
	DOM.addEl(countsTableContainer, countsTableDesc.container)
	DOM.addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createCountsPostinfBleedsTable = (data: Data, groups: PostinfBleedsGroups[]) => {
	const groupedCounts = Arr.summarise({
		data: data.postinf_bleed_dates,
		groups: groups,
		defaultCounts: { day7: 0, day14: 0, day30: 0 },
		getKey: getParticipantsKey,
		addRow: (row, counts) => {
			const isPresent = (val: any) => val !== null && val !== undefined && val !== ""
			if (isPresent(row.day7)) {
				counts.day7 += 1
			}
			if (isPresent(row.day14)) {
				counts.day14 += 1
			}
			if (isPresent(row.day30)) {
				counts.day30 += 1
			}
		},
	})

	const groupedCountsFlat = Arr.flattenMap(groupedCounts, [])

	const colSpec = getColSpecFromGroups(groups)
	colSpec.day7 = {}
	colSpec.day14 = {}
	colSpec.day30 = {}

	const countsAos = Arr.aoaToAos(groupedCountsFlat, Object.keys(colSpec))

	const countsTableDesc = createTableDesc()
	DOM.addDivWithText(countsTableDesc.desc, "Postnfection bleeds that have a date in redcap")
	addCountsExplanatoryNotes(countsTableDesc.desc, groups)

	const descDim = DOM.measureEl(countsTableDesc.container, window.innerWidth - SIDEBAR_WIDTH_PX, window.innerHeight)

	const tableEl = Table.createTableFromAos({
		aos: countsAos,
		colSpecInit: colSpec,
		title: "Postinfection bleed counts",
		defaults: { width: Math.max(100, descDim[0] / Object.keys(colSpec).length) },
		getTableHeightInit: () => window.innerHeight - DOM.SCROLLBAR_WIDTHS[0] - descDim[1],
	})

	const countsTableContainer = DOM.createDiv()
	DOM.addEl(countsTableContainer, countsTableDesc.container)
	DOM.addEl(countsTableContainer, tableEl)

	return countsTableContainer
}

const createBleedsTable = (data: any, year: number) => {
	const tableEl = Table.createTableFromAos({
		aos: data.bleed_dates.filter((row: any) => {
			let result = row.year === year
			if (result) {
				const notMissing = (val: any) => val !== "" && val !== undefined && val !== null
				result =
					notMissing(row.flu_day_0) ||
					notMissing(row.flu_day_7) ||
					notMissing(row.flu_day_14) ||
					notMissing(row.flu_day_220)
				if (year > 2020) {
					result =
						result ||
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
	const tableEl = Table.createTableFromAos({
		aos: data.titres,
		colSpecInit: {
			pid: {},
			site: {},
			year: {},
			day: {},
			vax_inf: {},
			virus: { width: 250 },
			subtype: {},
			eggcell: { access: "virus_egg_cell" },
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
			vax2020: { access: "study_year_vac_2020" },
			vax2021: { access: "study_year_vac_2021" },
			vax2022: { access: "study_year_vac_2022" },
		},
		title: "Titres",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 2 - DOM.SCROLLBAR_WIDTHS[0],
		onFilterChange: onFilterChange,
	})

	return tableEl
}

const getTitreKey = (row: any, group: string) => {
	let result = null
	switch (group) {
		case "eggcell":
			{
				result = row.virus_egg_cell
			}
			break
		default:
			{
				result = getParticipantsKey(row, group)
			}
			break
	}
	return result
}

const createTitreGMTTable = (titreData: any[], groups: string[]) => {
	const titreSummary = Arr.summariseAos(
		{
			data: titreData,
			groups: groups,
			defaultCounts: { titres: 0, logtitreSum: 0 },
			getKey: getTitreKey,
			addRow: (row, counts) => {
				counts.titres += 1
				counts.logtitreSum += Math.log(row.titre)
			},
		},
		(row) => {
			row.logmean = row.logtitreSum / row.titres
			row.GMT = Math.exp(row.logmean)
		}
	)

	const colSpec: any = {}
	for (const group of groups) {
		colSpec[group] = {}
		if (group === "virus") {
			colSpec[group].width = 150
		}
	}
	colSpec.titres = {}
	colSpec.GMT = { format: (x: any) => x.toFixed(0) }

	const tableEl = Table.createTableFromAos({
		aos: titreSummary,
		colSpecInit: colSpec,
		title: "GMT",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - DOM.SCROLLBAR_WIDTHS[0],
	})

	return tableEl
}

const createTitreGMRTable = (titreData: any[], groups: string[]) => {
	const ratios = Arr.summariseAos({
		data: titreData,
		groups: ["pid"].concat(ALL_GMR_GROUPS),
		defaultCounts: { d0: null, d14: null },
		getKey: getTitreKey,
		addRow: (row, counts) => {
			switch (row.day) {
				case 0:
					counts.d0 = row.titre
					break
				case 14:
					counts.d14 = row.titre
					break
			}
		},
	})

	const ratioSummary = Arr.summariseAos(
		{
			data: ratios,
			groups: groups,
			defaultCounts: { ratios: 0, logratioSum: 0 },
			getKey: (row, group) => row[group],
			filter: (row) => row.d14 !== null && row.d0 !== null,
			addRow: (row, counts) => {
				counts.ratios += 1
				counts.logratioSum += Math.log(row.d14 / row.d0)
			},
		},
		(row) => {
			row.logmean = row.logratioSum / row.ratios
			row.GMR = Math.exp(row.logmean)
		}
	)

	const colSpec: any = {}
	for (const group of groups) {
		colSpec[group] = {}
		if (group === "virus") {
			colSpec[group].width = 150
		}
	}
	colSpec.ratios = {}
	colSpec.GMR = { format: (x: any) => x.toFixed(2) }

	const tableEl = Table.createTableFromAos({
		aos: ratioSummary,
		colSpecInit: colSpec,
		title: "GMR",
		getTableHeightInit: () => (window.innerHeight - TITRES_HELP_HEIGHT) / 4 - DOM.SCROLLBAR_WIDTHS[0],
	})

	return tableEl
}

const createTitrePlot = (data: any[], settings: TitresSettings) => {
	const container = DOM.createDiv()
	container.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX + DOM.SCROLLBAR_WIDTHS[1]}px)`
	container.style.overflowX = "scroll"

	const allTitres = [5, 10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240]

	const collectFacetSet = (facet: any) => {
		let result = Arr.unique(data.map((x) => getTitreKey(x, facet))).sort(Arr.generalSort)
		switch (facet) {
			case "subtype":
				result = result.sort(Arr.desiredOrderSort(["H1", "H3", "BVic", "BYam"]))
				break
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

	const xFacetSets = collectFacetSets(settings.xFacets)
	const yFacetSets = collectFacetSets(settings.yFacets)
	const xAxisTicks = collectFacetSet(settings.xAxis)

	const plot = Plot.beginPlot({
		width: 200,
		widthType: "tick",
		height: 400,
		heightType: "facet",
		padAxis: { l: 70, r: 10, t: 30, b: 50 },
		padData: { l: 25, r: 50, t: 25, b: 25 },
		padFacet: 50,
		scaledXMin: -0.5,
		scaledXMax: settings.xAxis === "day" ? xAxisTicks.length + 0.5 : xAxisTicks.length - 0.5,
		yMin: 5,
		yMax: 10240,
		scaleYData: Math.log,
		scaleXData: (x) => {
			let result = xAxisTicks.indexOf(x)
			switch (settings.xAxis) {
				case "day":
					if (x >= 220) {
						result += 1
					}
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

	const xPxStep = plot.allXTicksXCoords[1] - plot.allXTicksXCoords[0]
	const yPxStep = plot.allYTicksYCoords[0] - plot.allYTicksYCoords[1]

	const gapX = 10
	const boxWidth = xPxStep / 2 - gapX
	const distWidth = boxWidth

	const getLocationInfo = (row: any) => {
		let result = null
		if (row.titre !== null) {
			const rowYFacets = settings.yFacets.map((yFacet) => getTitreKey(row, yFacet))
			const rowXFacets = settings.xFacets.map((xFacet) => getTitreKey(row, xFacet))
			const facetID = rowXFacets.concat(rowYFacets).join("_")

			const rowXAxis = getTitreKey(row, settings.xAxis)
			const stripID = facetID + "__" + rowXAxis
			const regionID = stripID + "___" + row.titre

			result = {
				xFacets: rowXFacets,
				yFacets: rowYFacets,
				xAxis: rowXAxis,
				facetID: facetID,
				stripID: stripID,
				regionID: regionID,
			}
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
	for (const row of data) {
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
	const regionPointsFilled = {} as any
	for (const row of data) {
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
			const pointAlpha = lineAlpha

			// TODO(sen) Have jitter be the same per individual
			const regionCount = plotCounts[rowLocationInfo.regionID]

			if (regionPointsFilled[rowLocationInfo.regionID] === undefined) {
				regionPointsFilled[rowLocationInfo.regionID] = 0
			}

			let yJitter = 0
			let xJitter = 0

			const pointSize = 5
			const pointHalfSize = pointSize / 2

			if (regionCount > 1) {
				if (regionCount < 5) {
					const pointPad = 1
					const pointAndPad = pointSize + 2 * pointPad
					const startOffset = -regionCount * 0.5 * pointAndPad
					const thisPointOffset = regionPointsFilled[rowLocationInfo.regionID] * pointAndPad
					xJitter = startOffset + thisPointOffset + pointHalfSize
				} else {
					const jitterHalfWidth = boxWidth / 4
					xJitter = Rand.unif(-jitterHalfWidth, jitterHalfWidth)
					const jitterHalfHeight = yPxStep / 4
					yJitter = Rand.unif(-jitterHalfHeight, jitterHalfHeight)
				}
			}

			regionPointsFilled[rowLocationInfo.regionID] += 1

			const yCoord = plot.scaleYToPx(row.titre, rowLocationInfo.yFacets) + yJitter
			const xCoord = plot.scaleXToPx(rowLocationInfo.xAxis, rowLocationInfo.xFacets) + xJitter
			const pointCol = lineColBase + pointAlpha
			Plot.drawRect(
				plot.renderer,
				{
					l: xCoord - pointHalfSize,
					r: xCoord + pointHalfSize,
					t: yCoord - pointHalfSize,
					b: yCoord + pointHalfSize,
				},
				pointCol
			)
		}
	}

	const boxLineThiccness = 2
	const boxplotCol = "#ffa600"
	const distColor = "#de61a8"
	const altColor = "#000000"

	const boxplotMeanCol = "#ffa6ff"

	// TODO(sen) Lines

	if (boxWidth >= 15) {
		const summary = Arr.summariseAos({
			data: data,
			groups: settings.xFacets.concat(settings.yFacets).concat([settings.xAxis]),
			defaultCounts: () => ({
				titres: [] as number[],
				titreCounts: allTitres.reduce((acc, titre) => {
					acc[titre] = 0
					return acc
				}, {} as any),
			}),
			getKey: getTitreKey,
			addRow: (row, summ) => {
				summ.titres.push(row.titre)
				if (summ.titreCounts[row.titre] === undefined) {
					summ.titreCounts[row.titre] = 0
				}
				summ.titreCounts[row.titre] += 1
			},
		})

		for (const summaryRow of summary) {
			const xVal = summaryRow[settings.xAxis]
			const xFacets = settings.xFacets.map((xFacet) => summaryRow[xFacet])
			const xCoord = plot.scaleXToPx(xVal, xFacets)

			const yFacets = settings.yFacets.map((yFacet) => summaryRow[yFacet])
			const yVals = summaryRow.titres.map((titre: number) => plot.scaleYToPx(titre, yFacets))

			const stats = Plot.getBoxplotStats(yVals)

			if (stats !== null) {
				Plot.addBoxplot(plot, stats, xCoord, boxWidth, boxplotCol, altColor, boxplotMeanCol, boxLineThiccness)
			}

			const titreCountMax = Arr.max(Object.values(summaryRow.titreCounts))
			const titresSorted = Object.keys(summaryRow.titreCounts)
				.map((x) => parseInt(x))
				.sort((a, b) => a - b)
			const titreCounts01: any = []
			for (const key of titresSorted) {
				titreCounts01.push(summaryRow.titreCounts[key] / titreCountMax)
			}

			const firstNonZero = titreCounts01.findIndex((x: any) => x !== 0)
			const lastNonZero = titreCounts01.findLastIndex((x: any) => x !== 0)

			const countTextCol = "#bfbdb6"
			let prevBarRight = null
			for (let count01Index = firstNonZero; count01Index <= lastNonZero; count01Index += 1) {
				const count01 = titreCounts01[count01Index]
				const titre = titresSorted[count01Index]
				const count = summaryRow.titreCounts[titre]
				const yCoord = plot.scaleYToPx(titre, yFacets)

				const barRight = xCoord + boxLineThiccness + distWidth * count01

				let down = yPxStep / 2
				let up = down
				if (count01Index == 0) {
					down = down / 2
				} else if (count01Index == allTitres.length - 1) {
					up = up / 2
				}

				Plot.drawDoubleLine(
					plot.renderer,
					barRight,
					yCoord - up,
					barRight,
					yCoord + down,
					distColor,
					altColor,
					boxLineThiccness,
					[],
					true
				)

				if (prevBarRight !== null) {
					const halfThicc = boxLineThiccness / 2
					let vLeft = prevBarRight
					let vRight = barRight
					if (vLeft > vRight) {
						const temp = vLeft
						vLeft = vRight
						vRight = temp
					}
					const hlineY = yCoord + down
					Plot.drawLine(
						plot.renderer,
						vLeft - halfThicc,
						hlineY,
						vRight + halfThicc,
						hlineY,
						distColor,
						boxLineThiccness,
						[]
					)
				}

				Plot.drawText(
					plot.renderer,
					`${count}`,
					barRight - boxLineThiccness,
					yCoord,
					countTextCol,
					0,
					"middle",
					"end",
					altColor
				)

				prevBarRight = barRight
			}

			const totalCount = Arr.sum(Object.values(summaryRow.titreCounts))

			Plot.drawText(
				plot.renderer,
				`${totalCount}`,
				xCoord,
				plot.scaleYToPx(5, yFacets) + 5,
				countTextCol,
				0,
				"top",
				"center",
				altColor
			)
		}
	}

	DOM.addEl(container, plot.canvas as HTMLElement)
	return container
}

const createSurveyTable = (completions: { [key: string]: number[] }, data: Data, year: number) => {
	const tableContainer = DOM.createDiv()

	const tableEl = Table.createTableFromAos({
		aos: data.weekly_surveys.filter((row: any) => row.year === year && row.complete !== 0),
		colSpecInit: { pid: {}, site: {}, week: { access: "survey_index" }, date: {}, ari: {} },
		title: "Completed weekly surveys",
		forRow: (row) => {
			if (completions[row.pid] === undefined) {
				completions[row.pid] = []
			}
			completions[row.pid].push(row.survey_index)
		},
	})

	DOM.addEl(tableContainer, tableEl)
	return tableContainer
}

const createCompletionsTable = (completions: { [key: string]: number[] }) => {
	const collapsed = []
	for (let [pid, completedSurveys] of Object.entries(completions)) {
		completedSurveys = completedSurveys.sort((a, b) => a - b)
		let collapsedCompletions = ""
		if (completedSurveys.length > 0) {
			collapsedCompletions += completedSurveys[0]
			if (completedSurveys.length > 1) {
				let prev = completedSurveys[0]
				let currentStreak = 1
				for (let completeIndex = 1; completeIndex < completedSurveys.length; completeIndex += 1) {
					const compl = completedSurveys[completeIndex]

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
							const lastNotShown = prev + currentStreak - 1
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

	const tableEl = Table.createTableFromAos({
		aos: collapsed,
		colSpecInit: { pid: {}, completions: { width: 300 } },
		title: "Collapsed completions",
	})

	const tableContainer = DOM.createDiv()
	DOM.addEl(tableContainer, tableEl)

	return tableContainer
}

const fetchData = async (password: string) => {
	let success = true
	let data = {}

	if (password === "" || password === null || password === undefined || !isString(password)) {
		success = false
	} else {
		const address = "https://reports2.hcwflustudy.com/api2/" + password + "/data.json"
		try {
			const resp = await fetch(address)
			data = await resp.json()
		} catch (e) {
			success = false
			console.error(e)
		}
	}

	return { success: success, data: <Data>data }
}

const getDataPageFromURL = () => {
	const path = window.location.pathname.slice(1)
	let result: DataPageID = "counts"
	if (ALL_DATAPAGE_IDS.includes(path)) {
		result = <DataPageID>path
	} else {
		window.history.replaceState(null, "", result)
	}
	return result
}

type CountsSettings = {
	table: CountTableID
	groupsRecords: RecordGroups[]
	groupsBleeds: BleedsGroups[]
	groupsPostinfBleeds: PostinfBleedsGroups[]
}

type TitresSettings = {
	groupsGMTs: GMTGroups[]
	groupsGMRs: GMRGroups[]
	xFacets: TitreFacets[]
	yFacets: TitreFacets[]
	xAxis: TitreFacets
}

const getCountsPageURL = (settings: CountsSettings) => {
	const recordGroups = `record_groups=${settings.groupsRecords.join(",")}`
	const bleedsGroups = `bleeds_groups=${settings.groupsBleeds.join(",")}`
	const postinfBleedsGroups = `postinf_bleeds_groups=${settings.groupsPostinfBleeds.join(",")}`
	const result = `counts?table=${settings.table}&${recordGroups}&${bleedsGroups}&${postinfBleedsGroups}`
	return result
}

const getTitresPageURL = (settings: TitresSettings) => {
	const groupsGMTs = `groupsGMTs=${settings.groupsGMTs.join(",")}`
	const groupsGMRs = `groupsGMRs=${settings.groupsGMRs.join(",")}`
	const xFacets = `xFacets=${settings.xFacets.join(",")}`
	const yFacets = `yFacets=${settings.yFacets.join(",")}`
	const result = `titres?${groupsGMTs}&${groupsGMRs}&${xFacets}&${yFacets}&xAxis=${settings.xAxis}`
	return result
}

const getURLArrayParam = <T>(params: any, paramName: string, allowed: string[], def: T[]) => {
	let urlArr = def
	let needToFixAddress = false

	if (params.has(paramName)) {
		const parArr = params.getAll(paramName)
		const first = parArr[0].split(",")
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

	return { urlArr: urlArr, needToFixAddress: needToFixAddress }
}

const getURLSingleParam = <T>(params: any, paramName: string, allowed: any[], def: T) => {
	let urlParam = def
	let needToFixAddress = false

	if (params.has(paramName)) {
		const allParams = params.getAll(paramName)
		let toCheck: any = allParams[0]
		if (isNumber(allowed[0])) {
			toCheck = parseInt(allParams[0])
		}
		const paramIsValid = allowed.includes(toCheck)
		if (paramIsValid) {
			urlParam = <T>toCheck
		}

		if (allParams.length > 1 || !paramIsValid) {
			needToFixAddress = true
		}
	} else {
		needToFixAddress = true
	}

	return { urlParam: urlParam, needToFixAddress: needToFixAddress }
}

const getCountSettingsFromURL = (def: CountsSettings) => {
	let urlTable = def.table
	let urlGroupsRecords = def.groupsRecords
	let urlGroupsBleeds = def.groupsBleeds
	let urlGroupsPostinfBleeds = def.groupsPostinfBleeds

	if (window.location.pathname === "/counts") {
		const params = new URLSearchParams(window.location.search)

		const tableRes = getURLSingleParam(params, "table", ALL_COUNTS_TABLES, urlTable)
		urlTable = tableRes.urlParam

		const recordGroupsRes = getURLArrayParam(params, "record_groups", ALL_RECORD_GROUPS, urlGroupsRecords)
		urlGroupsRecords = recordGroupsRes.urlArr

		const bleedGroupsRes = getURLArrayParam(params, "bleeds_groups", ALL_BLEEDS_GROUPS, urlGroupsBleeds)
		urlGroupsBleeds = bleedGroupsRes.urlArr

		const postinfBleedGroupsRes = getURLArrayParam(
			params,
			"postinf_bleeds_groups",
			ALL_POSTINF_BLEEDS_GROUPS,
			urlGroupsPostinfBleeds
		)
		urlGroupsPostinfBleeds = postinfBleedGroupsRes.urlArr

		const needToFixAddress =
			tableRes.needToFixAddress ||
			recordGroupsRes.needToFixAddress ||
			bleedGroupsRes.needToFixAddress ||
			postinfBleedGroupsRes.needToFixAddress

		if (needToFixAddress) {
			window.history.replaceState(
				null,
				"",
				getCountsPageURL({
					table: urlTable,
					groupsRecords: urlGroupsRecords,
					groupsBleeds: urlGroupsBleeds,
					groupsPostinfBleeds: urlGroupsPostinfBleeds,
				})
			)
		}
	}

	const result: CountsSettings = {
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
		const params = new URLSearchParams(window.location.search)

		const gmtGroupsRes = getURLArrayParam(params, "groupsGMTs", ALL_GMT_GROUPS, urlGroupsGMTs)
		urlGroupsGMTs = gmtGroupsRes.urlArr

		const gmrGroupsRes = getURLArrayParam(params, "groupsGMRs", ALL_GMT_GROUPS, urlGroupsGMRs)
		urlGroupsGMRs = gmrGroupsRes.urlArr

		const xFacetsRes = getURLArrayParam(params, "xFacets", ALL_TITRE_FACETS, urlXFacets)
		urlXFacets = xFacetsRes.urlArr

		const yFacetsRes = getURLArrayParam(params, "yFacets", ALL_TITRE_FACETS, urlYFacets)
		urlYFacets = yFacetsRes.urlArr

		const xAxisRes = getURLSingleParam(params, "xAxis", ALL_TITRE_FACETS, urlXAxis)
		urlXAxis = xAxisRes.urlParam

		const needToFixAddress =
			gmtGroupsRes.needToFixAddress ||
			gmrGroupsRes.needToFixAddress ||
			xFacetsRes.needToFixAddress ||
			yFacetsRes.needToFixAddress ||
			xAxisRes.needToFixAddress
		if (needToFixAddress) {
			window.history.replaceState(
				null,
				"",
				getTitresPageURL({
					groupsGMTs: urlGroupsGMTs,
					groupsGMRs: urlGroupsGMRs,
					xFacets: urlXFacets,
					yFacets: urlYFacets,
					xAxis: urlXAxis,
				})
			)
		}
	}

	const result: TitresSettings = {
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
		const params = new URLSearchParams(window.location.search)

		const yearRes = getURLSingleParam(params, "year", YEARS, urlYear)
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
		const params = new URLSearchParams(window.location.search)

		const yearRes = getURLSingleParam(params, "year", YEARS, urlYear)
		urlYear = yearRes.urlParam

		if (yearRes.needToFixAddress) {
			window.history.replaceState(null, "", `weekly-surveys?year=${urlYear}`)
		}
	}

	return urlYear
}

const createLoadingPage = () => {
	const loading = DOM.createDiv()
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
	const container = DOM.createDiv()
	const label = DOM.addDiv(container)
	const input = DOM.addEl(container, DOM.createEl("input"))
	const button = DOM.addDiv(container)
	const buttonText = DOM.addDiv(button)
	const errorText = DOM.addDiv(container)

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

	button.addEventListener("click", async () => {
		if (buttonText.textContent !== "...") {
			errorText.textContent = ""
			if (input.value === "") {
				errorText.textContent = "Password is empty"
			} else {
				buttonText.textContent = "..."
				const fetchResult = await fetchData(input.value)
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
	participants: any[]
	withdrawn: any[]
	bleed_dates: any[]
	consent: any[]
	weekly_surveys: any[]
	postinf_bleed_dates: any[]
	titres: any[]

	baseline_nonsense: any[]
	bled_d14_no_vax_record: any[]
	bleed_dates_wrong_order: any[]
	bleed_no_consent_covid: any[]
	bleed_no_consent: any[]
	consent_conflicts: any[]
	covid_bleeds_no_vax: any[]
	covid_vax_dates_not_ascending: any[]
	covid_vax_missing_brand: any[]
	covid_vax_missing_dates: any[]
	missing_baseline: any[]
	missing_bleed_dates: any[]
	missing_covax_dose: any[]
	missing_vaccination_history: any[]
	missing_vaccination_records: any[]
	serology_no_vax: any[]
	swabs_missing_date: any[]
	withdrawn_missing_date: any[]
}

type Pages = {
	participants: ParticipantsSettings
	"weekly-surveys": WeeklySurveySettings
	bleeds: BleedsSettings
	counts: CountsSettings
	titres: TitresSettings
	problems: ProblemsSettings
}

type ProblemsSettings = Record<string, unknown>
type ParticipantsSettings = Record<string, unknown>
type WeeklySurveySettings = { year: YearID }
type BleedsSettings = { year: YearID }

const updatePageFromURL = (pages: Pages): DataPageID => {
	let page = getDataPageFromURL()
	switch (page) {
		case "participants":
			pages.participants = {}
			break
		case "problems":
			pages.problems = {}
			break
		case "counts":
			pages.counts = getCountSettingsFromURL(pages.counts)
			break
		case "bleeds":
			pages.bleeds.year = getBleedsYearFromURL(pages.bleeds.year)
			break
		case "weekly-surveys":
			pages["weekly-surveys"].year = getSurveysYearFromURL(pages["weekly-surveys"].year)
			break
		case "titres":
			pages.titres = getTitresSettingsFromURL(pages.titres)
			break
		default:
			console.error("unexpected page:", page)
			page = "counts"
			break
	}
	return page
}

const SWITCH_COLORS = {
	normal: "var(--color-background)",
	hover: "var(--color-background2)",
	selected: "var(--color-selected)",
}

const createSidebar = (
	activePage: DataPageID,
	pageSpecific: HTMLElement,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	const sidebar = DOM.createDiv()
	sidebar.style.width = SIDEBAR_WIDTH_PX + "px"
	sidebar.style.height = "100vh"
	sidebar.style.flexShrink = "0"
	sidebar.style.display = "flex"
	sidebar.style.flexDirection = "column"
	sidebar.style.justifyContent = "space-between"
	sidebar.style.overflowX = "hidden"
	sidebar.style.overflowY = "scroll"

	const top = DOM.addDiv(sidebar)

	const linksContainer = DOM.addEl(
		top,
		Switch.createSwitch({
			type: "toggleOneNonNullable",
			getValue: () => activePage,
			setValue: onDatapageChange,
			opts: <DataPageID[]>ALL_DATAPAGE_IDS,
			colors: SWITCH_COLORS,
			name: "Pages",
		})
	)
	linksContainer.style.marginBottom = "20px"

	DOM.addEl(top, pageSpecific)

	const bottom = DOM.addDiv(sidebar)
	const logout = DOM.addDiv(bottom)
	logout.textContent = "Logout"
	logout.style.cursor = "pointer"
	logout.style.marginTop = "20px"
	logout.addEventListener("mouseover", () => (logout.style.backgroundColor = "var(--color-selected)"))
	logout.addEventListener("mouseleave", () => (logout.style.backgroundColor = "inherit"))
	logout.addEventListener("click", () => {
		localStorage.removeItem("password")
		onLogout()
	})

	return sidebar
}

const createDatapageContainer = () => {
	const container = DOM.createDiv()
	container.style.display = "flex"
	return container
}

const createParticipantsPage = (data: Data, onDatapageChange: (page: DataPageID) => void, onLogout: () => void) => {
	const page = createDatapageContainer()
	const settings = DOM.createDiv()

	DOM.addEl(page, createSidebar("participants", settings, onDatapageChange, onLogout))

	const container = DOM.addDiv(page)
	container.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	const table = DOM.addDiv(container)
	table.style.maxWidth = container.style.width

	const tableEl = Table.createTableFromAos({
		aos: data.participants,
		colSpecInit: {
			pid: {},
			site: {},
			email: {
				width: 450,
				format: (x) => x?.toLowerCase(),
				filterValProcess: (val: string) => val.toLowerCase(),
			},
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
	DOM.addEl(table, tableEl)

	return page
}

const createCountsTable = (data: Data, settings: CountsSettings) => {
	let tableEl = DOM.createDiv()
	switch (settings.table) {
		case "records":
			tableEl = createCountsRecordsTable(data, settings.groupsRecords)
			break

		case "routine-bleeds":
			tableEl = createCountsBleedsTable(data, settings.groupsBleeds)
			break

		case "postinfection-bleeds":
			tableEl = createCountsPostinfBleedsTable(data, settings.groupsPostinfBleeds)
			break

		default:
			console.error("unexpected counts table name", settings.table)
	}
	return tableEl
}

const createCountsSwitch = (parent: HTMLElement, data: Data, settings: CountsSettings) => {
	let switchEl = DOM.createDiv()
	let createTable = () => DOM.createDiv()

	switch (settings.table) {
		case "records":
			{
				createTable = () => createCountsRecordsTable(data, settings.groupsRecords)
				switchEl = Switch.createSwitch({
					type: "toggleMany",
					state: settings.groupsRecords,
					opts: <RecordGroups[]>ALL_RECORD_GROUPS,
					onUpdate: () => {
						window.history.pushState(null, "", getCountsPageURL(settings))
						DOM.replaceChildren(parent, createTable())
					},
					colors: SWITCH_COLORS,
					name: "Groups",
				})
			}
			break

		case "routine-bleeds":
			{
				createTable = () => createCountsBleedsTable(data, settings.groupsBleeds)
				switchEl = Switch.createSwitch({
					type: "toggleMany",
					state: settings.groupsBleeds,
					opts: <BleedsGroups[]>ALL_BLEEDS_GROUPS,
					onUpdate: () => {
						window.history.pushState(null, "", getCountsPageURL(settings))
						DOM.replaceChildren(parent, createTable())
					},
					colors: SWITCH_COLORS,
					name: "Groups",
				})
			}
			break

		case "postinfection-bleeds":
			{
				createTable = () => createCountsPostinfBleedsTable(data, settings.groupsPostinfBleeds)
				switchEl = Switch.createSwitch({
					type: "toggleMany",
					state: settings.groupsPostinfBleeds,
					opts: <PostinfBleedsGroups[]>ALL_POSTINF_BLEEDS_GROUPS,
					onUpdate: () => {
						window.history.pushState(null, "", getCountsPageURL(settings))
						DOM.replaceChildren(parent, createTable())
					},
					colors: SWITCH_COLORS,
					name: "Groups",
				})
			}
			break

		default:
			console.error("unexpected counts table name", settings.table)
	}

	return switchEl
}

const createCountsPage = (
	data: Data,
	settings: CountsSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	const counts = DOM.createDiv()
	counts.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	counts.style.overflowX = "hidden"
	counts.style.display = "flex"

	const tableParent = DOM.addDiv(counts)
	tableParent.style.maxWidth = counts.style.width
	DOM.addEl(tableParent, createCountsTable(data, settings))

	const switchParent = DOM.createDiv()
	DOM.addEl(switchParent, createCountsSwitch(tableParent, data, settings))

	const updateTableAndSwitch = () => {
		DOM.replaceChildren(tableParent, createCountsTable(data, settings))
		DOM.replaceChildren(switchParent, createCountsSwitch(tableParent, data, settings))
	}

	const switchContainer = DOM.createDiv()

	const tableSwitch = DOM.addEl(
		switchContainer,
		Switch.createSwitch({
			type: "toggleOneNonNullable",
			getValue: () => settings.table,
			setValue: (opt) => (settings.table = opt),
			opts: <CountTableID[]>ALL_COUNTS_TABLES,
			onUpdate: () => {
				window.history.pushState(null, "", getCountsPageURL(settings))
				updateTableAndSwitch()
			},
			colors: SWITCH_COLORS,
			name: "Table",
		})
	)
	tableSwitch.style.marginBottom = "20px"

	DOM.addEl(switchContainer, switchParent)

	const page = createDatapageContainer()
	DOM.addEl(page, createSidebar("counts", switchContainer, onDatapageChange, onLogout))
	DOM.addEl(page, counts)

	return page
}

const createBleedsPage = (
	data: Data,
	settings: BleedsSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	const bleeds = DOM.createDiv()
	bleeds.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	bleeds.style.display = "flex"

	const tableParent = DOM.addDiv(bleeds)
	tableParent.style.maxWidth = bleeds.style.width

	DOM.addEl(tableParent, createBleedsTable(data, settings.year))

	const yearSwitch = Switch.createSwitch({
		type: "toggleOneNonNullable",
		getValue: () => settings.year,
		setValue: (year) => (settings.year = year),
		opts: <YearID[]>YEARS,
		onUpdate: () => {
			window.history.pushState(null, "", `bleeds?year=${settings.year}`)
			DOM.replaceChildren(tableParent, createBleedsTable(data, settings.year))
		},
		name: "Year",
		colors: SWITCH_COLORS,
	})

	const page = createDatapageContainer()
	DOM.addEl(page, createSidebar("bleeds", yearSwitch, onDatapageChange, onLogout))
	DOM.addEl(page, bleeds)

	return page
}

const createWeeklySurveysPage = (
	data: Data,
	settings: WeeklySurveySettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	const hscrollContainer = DOM.createDiv()
	hscrollContainer.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	hscrollContainer.style.overflowX = "scroll"
	hscrollContainer.style.overflowY = "hidden"

	const container = DOM.addDiv(hscrollContainer)
	container.style.display = "flex"
	container.style.height = `calc(100vh - ${DOM.SCROLLBAR_WIDTHS[0]}px)`

	const getDates = (weekCount: number, start: string, end: string, send: string) => {
		const result: any[] = []
		const startDate = new Date(start)
		const endDate = new Date(end)
		const sendDate = new Date(send)
		for (let index = 0; index < weekCount; index += 1) {
			const row = {
				week: `${index + 1}`,
				start: formatDate(addDays(startDate, index * 7)),
				end: formatDate(addDays(endDate, index * 7)),
				send: formatDate(addDays(sendDate, index * 7)),
			}
			result.push(row)
		}
		return result
	}

	const surveyDates = {
		2020: Table.createTableFromAos({
			aos: getDates(32, "2020-04-06", "2020-04-12", "2020-04-13"),
			colSpecInit: { week: {}, start: {}, end: {}, send: {} },
			title: "Weekly survey dates 2020",
		}),

		2021: Table.createTableFromAos({
			aos: getDates(52, "2021-01-04", "2021-01-10", "2021-01-11"),
			colSpecInit: { week: {}, start: {}, end: {}, send: {} },
			title: "Weekly survey dates 2021",
		}),

		2022: Table.createTableFromAos({
			aos: getDates(52, "2022-01-03", "2022-01-09", "2022-01-10"),
			colSpecInit: { week: {}, start: {}, end: {}, send: {} },
			title: "Weekly survey dates 2022",
		}),
	}

	const datesTableParent = DOM.addDiv(container)
	const surveysTableParent = DOM.addDiv(container)
	const completionsTableParent = DOM.addDiv(container)

	const updateTables = (year: YearID) => {
		DOM.replaceChildren(datesTableParent, surveyDates[year])
		const completions = {}
		DOM.replaceChildren(surveysTableParent, createSurveyTable(completions, data, year))
		DOM.replaceChildren(completionsTableParent, createCompletionsTable(completions))
	}

	updateTables(settings.year)

	const yearSwitch = Switch.createSwitch({
		type: "toggleOneNonNullable",
		getValue: () => settings.year,
		setValue: (year) => (settings.year = year),
		opts: <YearID[]>YEARS,
		onUpdate: () => {
			window.history.pushState(null, "", `weekly-surveys?year=${settings.year}`)
			updateTables(settings.year)
		},
		name: "Year",
		colors: SWITCH_COLORS,
	})

	const page = createDatapageContainer()
	DOM.addEl(page, createSidebar("weekly-surveys", yearSwitch, onDatapageChange, onLogout))
	DOM.addEl(page, hscrollContainer)

	return page
}

const createTitresPage = (
	data: Data,
	settings: TitresSettings,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	const container2 = DOM.createDiv()
	container2.style.overflowX = "hidden"
	container2.style.overflowY = "scroll"
	container2.style.height = "100vh"
	container2.style.width = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`

	const help = DOM.addDiv(container2)

	const fillHelp = () => {
		DOM.removeChildren(help)
		DOM.addDivWithText(
			help,
			"GMT, GMR tables and the titre plot only use the data displayed in the Titres table (so you can filter the titres table and change everything else on the page)."
		)
		DOM.addDivWithText(
			help,
			"Boxplots: minimum - quartile 25 - quartile 75 - maximum. Solid midline: median. Circle: mean (vertical line - 95% CI for mean). Right side: histogram. Numbers: titre measurement counts."
		)
		addGroupsExplanatoryNotes(
			help,
			Arr.unique(
				// @ts-ignore // TODO(sen) Fix
				settings.groupsGMRs.concat(settings.groupsGMTs).concat(settings.xFacets).concat(settings.yFacets)
			)
		)
	}
	fillHelp()

	const top = DOM.addDiv(container2)
	top.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX + DOM.SCROLLBAR_WIDTHS[1]}px)`
	top.style.flex = "1 0"
	top.style.display = "flex"
	top.style.overflow = "hidden"
	const bottom = DOM.addEl(container2, <HTMLElement>top.cloneNode(true))

	const left = DOM.addDiv(top)
	left.style.maxWidth = `calc((100vw - ${SIDEBAR_WIDTH_PX + DOM.SCROLLBAR_WIDTHS[1]}px) / 2)`
	left.style.flex = "1 0"
	left.style.overflow = "hidden"
	const right = DOM.addEl(top, <HTMLElement>left.cloneNode(true))

	const tableParent = DOM.addDiv(left)
	tableParent.style.flex = "1 0"
	tableParent.style.display = "flex"

	const plotParent = DOM.addEl(bottom, <HTMLElement>tableParent.cloneNode(true))

	const gmtTableParent = DOM.addEl(right, <HTMLElement>tableParent.cloneNode(true))
	gmtTableParent.style.height = "50%"

	const gmrTableParent = DOM.addEl(right, <HTMLElement>tableParent.cloneNode(true))

	let latestFilteredData: any[] = []

	const updateGMTs = () =>
		DOM.replaceChildren(gmtTableParent, createTitreGMTTable(latestFilteredData, settings.groupsGMTs))
	const updateGMRs = () =>
		DOM.replaceChildren(gmrTableParent, createTitreGMRTable(latestFilteredData, settings.groupsGMRs))
	const updatePlot = () => DOM.replaceChildren(plotParent, createTitrePlot(latestFilteredData, settings))

	DOM.addEl(
		tableParent,
		createTitreTable(data, (filteredData) => {
			latestFilteredData = filteredData
			updateGMTs()
			updateGMRs()
			updatePlot()
		})
	)

	const settingsEl = DOM.createDiv()

	const gmtGroupsSwitch = Switch.createSwitch({
		type: "toggleMany",
		state: settings.groupsGMTs,
		opts: <GMTGroups[]>ALL_GMT_GROUPS,
		onUpdate: () => {
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updateGMTs()
		},
		name: "GMT groups",
		colors: SWITCH_COLORS,
	})

	DOM.addEl(settingsEl, gmtGroupsSwitch)

	const gmrGroupsSwitch = Switch.createSwitch({
		type: "toggleMany",
		state: settings.groupsGMRs,
		opts: <GMRGroups[]>ALL_GMR_GROUPS,
		onUpdate: () => {
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updateGMRs()
		},
		name: "GMR groups",
		colors: SWITCH_COLORS,
	})
	gmrGroupsSwitch.style.marginTop = "20px"

	DOM.addEl(settingsEl, gmrGroupsSwitch)

	const xAxisSwitch = Switch.createSwitch({
		type: "toggleOneNonNullable",
		getValue: () => settings.xAxis,
		setValue: (opt) => (settings.xAxis = opt),
		opts: <TitreFacets[]>ALL_TITRE_FACETS,
		onUpdate: () => {
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
		name: "X axis",
		colors: SWITCH_COLORS,
	})
	xAxisSwitch.style.marginTop = "20px"

	DOM.addEl(settingsEl, xAxisSwitch)

	const xFacetsSwitch = Switch.createSwitch({
		type: "toggleMany",
		state: settings.xFacets,
		opts: <TitreFacets[]>ALL_TITRE_FACETS,
		onUpdate: () => {
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
		name: "X facets",
		colors: SWITCH_COLORS,
	})
	xFacetsSwitch.style.marginTop = "20px"

	DOM.addEl(settingsEl, xFacetsSwitch)

	const yFacetsSwitch = Switch.createSwitch({
		type: "toggleMany",
		state: settings.yFacets,
		opts: <TitreFacets[]>ALL_TITRE_FACETS,
		onUpdate: () => {
			window.history.pushState(null, "", getTitresPageURL(settings))
			fillHelp()
			updatePlot()
		},
		name: "Y facets",
		colors: SWITCH_COLORS,
	})
	yFacetsSwitch.style.marginTop = "20px"

	DOM.addEl(settingsEl, yFacetsSwitch)

	const page = createDatapageContainer()
	DOM.addEl(page, createSidebar("titres", settingsEl, onDatapageChange, onLogout))
	DOM.addEl(page, container2)

	return page
}

const createProblemsPage = (data: Data, onDatapageChange: (page: DataPageID) => void, onLogout: () => void) => {
	const container = DOM.createDiv()
	container.style.maxHeight = "100vh"
	container.style.maxWidth = `calc(100vw - ${SIDEBAR_WIDTH_PX}px)`
	container.style.overflowY = "scroll"

	const helpEl = DOM.addDiv(container)
	DOM.addDivWithText(
		helpEl,
		"If a table is missing then no problems were found. Let me know (sen.khvorov@unimelb.edu.au) of any false flags."
	)
	DOM.addDivWithText(helpEl, "Missing data - fill if able, leave missing if not.")
	DOM.addDivWithText(helpEl, "List of tables:")
	DOM.addDivWithText(helpEl, "Baseline nonsense: DOB, weight or height don't make sense.")
	DOM.addDivWithText(helpEl, "Bleed dates wrong order: Successive bleed dates do not go in ascending order.")
	DOM.addDivWithText(helpEl, "Serology no vax: We have flu antibody titres but there is no corresponding vaccination record.")
	DOM.addDivWithText(
		helpEl,
		"Consent conflicts: Multiple consent forms filled and information on study group conflicts. We don't need the manual form in the presence of the electronic one. You can fix the wrong form or delete it."
	)
	DOM.addDivWithText(helpEl, "Covid bleed no vax: Covid bleed recorded but no covid vaccinations recorded.")
	DOM.addDivWithText(helpEl, "Missing baseline: baseline survey")
	DOM.addDivWithText(helpEl, "Missing bleed dates: routing bleed dates missing (but we have titres)")
	DOM.addDivWithText(helpEl, "Missing vaccination history: screening form")
	DOM.addDivWithText(helpEl, "Missing vaccination records: Missing study year vaccination records (vaccination form)")
	DOM.addDivWithText(helpEl, "Swabs missing date: missing swab dates")
	DOM.addDivWithText(helpEl, "Withdrawn missing dates: missing withdrawal dates")
	DOM.addDivWithText(
		helpEl,
		"Covid vax not ascending: Subsequent covid vaccination date is before the previous vaccination date"
	)
	DOM.addDivWithText(helpEl, "Covid vax missing dates: missing covid vaccination date")
	DOM.addDivWithText(helpEl, "Covid vax missing brand: missing covid vaccination brand")
	DOM.addDivWithText(helpEl, "Bled d14 no vax: We have a d14 titre but there is no vaccination record")
	DOM.addDivWithText(
		helpEl,
		"Missing covax dose: Non-withdrawn covid-consented participants missing covax info on dose 1, 2 or 3. Column `doses` shows the doses we have info for."
	)
	DOM.addDivWithText(helpEl, "Bleed no consent: No recorded flu consent but we have antibody titre.")
	DOM.addDivWithText(helpEl, "Bleed no consent covid: No recorded covid consent but we have antibody titre.")

	const tablesContainer = DOM.addDiv(container)
	tablesContainer.style.display = "flex"
	tablesContainer.style.flexWrap = "wrap"

	if (data.baseline_nonsense.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.baseline_nonsense,
				colSpecInit: {
					pid: {},
					site: {},
					dob: {},
					weight: {},
					height: {},
				},
				title: "Baseline nonsense",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.bleed_dates_wrong_order.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.bleed_dates_wrong_order,
				colSpecInit: {
					pid: {},
					site: {},
					year: {},
					"0": {},
					"7": {},
					"14": {},
					"220": {},
				},
				title: "Bleed dates wrong order",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.serology_no_vax.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.serology_no_vax,
				colSpecInit: { pid: {}, site: {}, year: {} },
				title: "Serology no vax",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.consent_conflicts.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.consent_conflicts,
				colSpecInit: {
					pid: {},
					year: {},
					site: {},
					fluManual: { access: "flu_manual" },
					fluElecVac: { access: "flu_electronic_vac" },
					fluElecUnvac: { access: "flu_electronic_unvac" },
					covidManual: { access: "covid_manual" },
					covidElec: { access: "covid_electronic" },
				},
				title: "Consent conflicts",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.covid_bleeds_no_vax.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.covid_bleeds_no_vax,
				colSpecInit: {
					pid: {},
					year: {},
					site: {},
					day: {},
					date: {},
				},
				title: "Covid bleeds no vax",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.missing_baseline.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.missing_baseline,
				colSpecInit: {
					pid: {},
					site: {},
					gender: {},
					dob: {},
					atsi: {},
					date_screening: {},
					email: { width: 300 },
					mobile: {},
					recruited: { access: "recruitment_year" },
					age_screening: {},
				},
				title: "Missing baseline",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.missing_bleed_dates.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.missing_bleed_dates,
				colSpecInit: {
					pid: {},
					site: {},
					year: {},
					day: {},
					date: {},
					virus: {},
					titre: {},
					subtype: {},
					eggcell: { access: "virus_egg_cell" },
				},
				title: "Missing bleed dates",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.missing_vaccination_history.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.missing_vaccination_history,
				colSpecInit: {
					pid: {},
					site: {},
					year: { access: "recruitment_year" },
				},
				title: "Missing vaccination history",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.missing_vaccination_records.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.missing_vaccination_records,
				colSpecInit: {
					pid: {},
					site: {},
					year: { access: "recruitment_year" },
				},
				title: "Missing vaccination records",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.swabs_missing_date.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.swabs_missing_date,
				colSpecInit: {
					pid: {},
					site: {},
					year: {},
					date: { access: "samp_date" },
					viruses: {},
				},
				title: "Swabs missing date",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.withdrawn_missing_date.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.withdrawn_missing_date,
				colSpecInit: {
					pid: {},
					site: {},
					year: { access: "redcap_project_year" },
					date: { access: "withdrawal_date" },
				},
				title: "Withdrawn missing date",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.covid_vax_dates_not_ascending.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.covid_vax_dates_not_ascending,
				colSpecInit: {
					pid: {},
					site: {},
					dose: {},
					date: {},
					date_prev: {},
					interval: {},
				},
				title: "Covid vax not ascending",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.covid_vax_missing_dates.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.covid_vax_missing_dates,
				colSpecInit: {
					pid: {},
					site: {},
					dose: {},
					date: {},
					brand: {},
				},
				title: "Covid vax missing dates",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.covid_vax_missing_brand.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.covid_vax_missing_brand,
				colSpecInit: {
					pid: {},
					site: {},
					dose: {},
					date: {},
					brand: {},
				},
				title: "Covid vax missing brand",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.bled_d14_no_vax_record.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.bled_d14_no_vax_record,
				colSpecInit: {
					pid: {},
					year: {},
					site: {},
				},
				title: "Bled d14 no vax",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.missing_covax_dose.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.missing_covax_dose,
				colSpecInit: {
					pid: {},
					site: {},
					doses: {},
				},
				title: "Missing covax dose",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.bleed_no_consent.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.bleed_no_consent,
				colSpecInit: { pid: {}, site: {}, year: {} },
				title: "Bleed no consent",
				getTableHeightInit: () => 500,
			})
		)
	}

	if (data.bleed_no_consent_covid.length > 0) {
		DOM.addEl(
			tablesContainer,
			Table.createTableFromAos({
				aos: data.bleed_no_consent_covid,
				colSpecInit: { pid: { width: 300 } },
				title: "Bleed no consent covid",
				getTableHeightInit: () => 500,
			})
		)
	}

	const page = createDatapageContainer()
	DOM.addEl(page, createSidebar("problems", DOM.createDiv(), onDatapageChange, onLogout))
	DOM.addEl(page, container)

	return page
}

const createDatapage = (
	page: DataPageID,
	pages: Pages,
	data: Data,
	onDatapageChange: (page: DataPageID) => void,
	onLogout: () => void
) => {
	// TODO(sen) Page Listeners
	let pageEl = DOM.createDiv()
	switch (page) {
		case "participants":
			pageEl = createParticipantsPage(data, onDatapageChange, onLogout)
			break
		case "counts":
			pageEl = createCountsPage(data, pages.counts, onDatapageChange, onLogout)
			break
		case "bleeds":
			pageEl = createBleedsPage(data, pages.bleeds, onDatapageChange, onLogout)
			break
		case "weekly-surveys":
			pageEl = createWeeklySurveysPage(data, pages["weekly-surveys"], onDatapageChange, onLogout)
			break
		case "titres":
			pageEl = createTitresPage(data, pages.titres, onDatapageChange, onLogout)
			break
		case "problems":
			pageEl = createProblemsPage(data, onDatapageChange, onLogout)
			break
		default:
			console.error("unexpected page:", page)
			break
	}
	return pageEl
}

const getURLFromPageInfo = (page: DataPageID, pages: Pages): string => {
	let result = ""
	switch (page) {
		case "participants":
			result = "participants"
			break
		case "counts":
			result = getCountsPageURL(pages.counts)
			break
		case "bleeds":
			result = `bleeds?year=${pages.bleeds.year}`
			break
		case "weekly-surveys":
			result = `weekly-surveys?year=${pages["weekly-surveys"].year}`
			break
		case "titres":
			result = getTitresPageURL(pages.titres)
			break
		case "problems":
			result = "problems"
			break
		default:
			console.error("unexpected page:", page)
			result = getCountsPageURL(pages.counts)
			break
	}
	return result
}

const goToPage = (domMain: HTMLElement, page: DataPageID, pages: Pages, data: Data, onLogout: () => void) => {
	DOM.replaceChildren(
		domMain,
		createDatapage(
			page,
			pages,
			data,
			(page) => {
				const newURL = getURLFromPageInfo(page, pages)
				window.history.pushState(null, "", newURL)
				goToPage(domMain, page, pages, data, onLogout)
			},
			onLogout
		)
	)
}

const goToCurrentURL = (domMain: HTMLElement, data: Data, onLogout: () => void) => {
	const defCountsSettings: CountsSettings = {
		table: "records",
		groupsRecords: ["site"],
		groupsBleeds: ["year"],
		groupsPostinfBleeds: ["year"],
	}
	const defYear = 2022
	const defTitresSettings: TitresSettings = {
		groupsGMTs: ["year", "day"],
		groupsGMRs: ["year"],
		xFacets: ["year", "eggcell"],
		yFacets: ["subtype"],
		xAxis: "day",
	}

	const pages: Pages = {
		participants: {},
		"weekly-surveys": { year: defYear },
		bleeds: { year: defYear },
		counts: defCountsSettings,
		titres: defTitresSettings,
		problems: {},
	}

	const updatePagesAndGoToCurrent = () => {
		const urlPage = updatePageFromURL(pages)
		goToPage(domMain, urlPage, pages, data, onLogout)
	}

	// NOTE(sen) Only called once
	globalThis.window.addEventListener("popstate", updatePagesAndGoToCurrent)
	updatePagesAndGoToCurrent()
}

const main = async () => {
	const historyChangeStateAndDispatchEvent = function (type: string) {
		//@ts-ignore // TODO(sen) Fix
		const orig: any = history[type]
		return function () {
			//@ts-ignore // TODO(sen) Fix
			const rv = orig.apply(this, arguments)
			const e: any = new Event(type)
			e.arguments = arguments
			globalThis.window.dispatchEvent(e)
			return rv
		}
	}

	history.pushState = historyChangeStateAndDispatchEvent("pushState")
	history.replaceState = historyChangeStateAndDispatchEvent("replaceState")

	// NOTE(sen) Attempt to login from local storage
	const domMain = document.getElementById("main")!
	const onPasswordFail = () => DOM.replaceChildren(domMain, createPasswordPage(onPasswordSuccess))
	const onPasswordSuccess = (data: Data) => goToCurrentURL(domMain, data, onPasswordFail)
	const password = localStorage.getItem("password")
	if (password === null) {
		onPasswordFail()
	} else {
		DOM.replaceChildren(domMain, createLoadingPage())
		const fetchResult = await fetchData(password)
		if (!fetchResult.success) {
			onPasswordFail()
		} else {
			onPasswordSuccess(fetchResult.data)
		}
	}
}

main()

// TODO(sen) Counts of swab results
// TODO(sen) Date filtering
// TODO(sen) Table sorting
// TODO(sen) Titre plots
