const createEl = document.createElement.bind(document)
const createDiv = () => createEl("div")
const addEl = <T1 extends HTMLElement, T2 extends HTMLElement>(parent: T1, child: T2) => {
	parent.appendChild(child)
	return child
}
const addDiv = (parent: HTMLElement) => addEl(parent, createDiv())
const createDivWithText = (text: string) => {
	const div = createDiv()
	div.textContent = text
	return div
}

type SwitchSpecBody<T> = {
	opts: T[]
	colors: { normal: string; hover: string; selected: string }
	name: string
	initClosed?: boolean
	help?: string
	optContainerStyle?: (container: HTMLDivElement) => void
	optElementStyle?: (el: HTMLDivElement) => void
	switchElementStyle?: (el: HTMLDivElement) => void
	optElementStorage?: HTMLDivElement[]
	onExpand?: (expanded: boolean) => void
}

const createSwitchCommon = <T>(
	spec: SwitchSpecBody<T>,
	optElementInit: (el: HTMLDivElement, opt: T, optIndex: number) => void,
	toggleOption: (event: MouseEvent, opt: T, index: number, el: HTMLDivElement, allEls: HTMLDivElement[]) => void
) => {
	const switchElement = createDiv()
	spec.switchElementStyle?.(switchElement)
	const optContainer = createDiv()
	spec.optContainerStyle?.(optContainer)

	const displayStyleWhenVisible = optContainer.style.display
	let optContainerDisplayed = spec.initClosed ?? true

	const labelContainer = addDiv(switchElement)
	labelContainer.style.display = "flex"
	labelContainer.style.justifyContent = "space-between"

	const label = addDiv(labelContainer)
	label.style.fontSize = "large"
	label.style.cursor = "pointer"
	label.style.paddingLeft = "5px"

	const setCollapse = (optContainerDisplayed: boolean) => {
		if (optContainerDisplayed) {
			optContainer.style.display = displayStyleWhenVisible
			label.textContent = spec.name + " ▲"
		} else {
			optContainer.style.display = "none"
			label.textContent = spec.name + " ▼"
		}
	}

	label.addEventListener("click", () => {
		optContainerDisplayed = !optContainerDisplayed
		setCollapse(optContainerDisplayed)
		spec.onExpand?.(optContainerDisplayed)
	})

	setCollapse(optContainerDisplayed)
	addEl(switchElement, optContainer)

	if (spec.help !== undefined) {
		const help = addDiv(labelContainer)
		help.textContent = "?"
		help.style.cursor = "pointer"
		help.style.paddingLeft = "10px"
		help.style.paddingRight = help.style.paddingLeft
		help.style.position = "relative"

		const helpText = addDiv(help)
		helpText.style.position = "absolute"
		helpText.style.right = "0px"
		helpText.style.backgroundColor = "var(--color-background2)"
		helpText.style.width = "150px"
		helpText.style.zIndex = "999"
		helpText.style.padding = "5px"
		helpText.style.whiteSpace = "pre-line"
		addEl(helpText, createDivWithText(spec.help))

		const helpDisplayWhenVisible = helpText.style.display
		helpText.style.display = "none"
		help.addEventListener("click", () => {
			if (helpText.style.display === "none") {
				helpText.style.display = helpDisplayWhenVisible
			} else {
				helpText.style.display = "none"
			}
		})
	}

	const allOptElements: HTMLDivElement[] = spec.optElementStorage ?? []
	for (let optIndex = 0; optIndex < spec.opts.length; optIndex++) {
		const opt = spec.opts[optIndex]
		const optElement = addDiv(optContainer)
		allOptElements.push(optElement)
		optElement.style.paddingTop = "5px"
		optElement.style.paddingBottom = "5px"
		optElement.style.cursor = "pointer"
		optElement.style.textAlign = "center"
		optElement.textContent = `${opt}`
		optElementInit(optElement, opt, optIndex)
		spec.optElementStyle?.(optElement)

		optElement.addEventListener("click", (event) => toggleOption(event, opt, optIndex, optElement, allOptElements))
	}

	return switchElement
}

type SwitchSpecHead<T> =
	| {
			type: "toggleOneNonNullable"
			getValue: () => T
			setValue: (opt: T) => void
			onUpdate?: () => void
	  }
	| {
			type: "toggleMany"
			state: T[]
			onUpdate?: () => void
	  }
	| {
			type: "gradient"
			getValue: (opt: T, optIndex: number) => number
			setValue: (opt: T, optIndex: number, value: number) => void
			onUpdate?: () => void
	  }

export type SwitchSpec<T> = SwitchSpecHead<T> & SwitchSpecBody<T>

export const createSwitch = <T>(spec: SwitchSpec<T>) => {
	let result: HTMLDivElement

	const optElementInitToggle = (opt: T, optElement: HTMLDivElement, isSelected: (opt: T) => boolean) => {
		optElement.style.backgroundColor = isSelected(opt) ? spec.colors.selected : spec.colors.normal
		optElement.addEventListener("mouseover", () => {
			if (!isSelected(opt)) {
				optElement.style.backgroundColor = spec.colors.hover
			}
		})
		optElement.addEventListener("mouseout", () => {
			if (!isSelected(opt)) {
				optElement.style.backgroundColor = spec.colors.normal
			}
		})
	}

	switch (spec.type) {
		case "toggleOneNonNullable":
			{
				const isSelected = (opt: T) => opt === spec.getValue()
				const toggleOption = (
					_event: MouseEvent,
					opt: T,
					_index: number,
					optElement: HTMLDivElement,
					allOptElements: HTMLDivElement[]
				) => {
					if (!isSelected(opt)) {
						spec.setValue(opt)
						for (const el of allOptElements) {
							el.style.backgroundColor = spec.colors.normal
						}
						optElement.style.backgroundColor = spec.colors.selected
						spec.onUpdate?.()
					}
				}
				result = createSwitchCommon(spec, (el, opt) => optElementInitToggle(opt, el, isSelected), toggleOption)
			}
			break

		case "toggleMany":
			{
				const allOpts = [...spec.opts]
				const isSelected = (opt: T) => spec.state.includes(opt)
				const toggleOption = (
					event: MouseEvent,
					opt: T,
					_indexInAll: number,
					optElement: HTMLDivElement,
					allOptElements: HTMLDivElement[]
				) => {
					if (event.ctrlKey) {
						allOptElements.map((optEl) => (optEl.style.backgroundColor = spec.colors.normal))
						optElement.style.backgroundColor = spec.colors.selected
						spec.state.length = 0
						spec.state.push(opt)
					} else if (event.shiftKey) {
						allOptElements.map((optEl) => (optEl.style.backgroundColor = spec.colors.selected))
						spec.state.length = 0
						allOpts.map((opt) => spec.state.push(opt))
					} else {
						if (isSelected(opt)) {
							const indexInState = spec.state.indexOf(opt)
							if (indexInState !== -1) {
								spec.state.splice(indexInState, 1)
							} else {
								console.error(`Switch opt ${opt} selected but not found in ${spec.state}`)
							}
							optElement.style.backgroundColor = spec.colors.normal
						} else {
							spec.state.push(opt)
							optElement.style.backgroundColor = spec.colors.selected
						}
					}
					spec.onUpdate?.()
				}

				spec.help = spec.help === undefined ? "" : (spec.help += "\n")
				spec.help += "ctrl+click = select one\nshift+click = select all"

				result = createSwitchCommon(spec, (el, opt) => optElementInitToggle(opt, el, isSelected), toggleOption)
			}
			break

		case "gradient":
			{
				const optElementInit = (el: HTMLDivElement, opt: T, index: number) => {
					const fromLeft = spec.getValue(opt, index)
					const fromLeftPercent = Math.round(fromLeft * 100)
					el.style.background = `linear-gradient(to right, ${spec.colors.selected} ${fromLeftPercent}%, ${spec.colors.normal} ${fromLeftPercent}%)`
				}
				const toggleOption = (event: MouseEvent, opt: T, optIndex: number, optElement: HTMLDivElement) => {
					const parent = <HTMLDivElement>event.target
					let fromLeft = event.offsetX / parent.offsetWidth
					if (event.ctrlKey) {
						fromLeft = 0
					} else if (event.shiftKey) {
						fromLeft = 1
					}
					const fromLeftPercent = Math.round(fromLeft * 100)
					optElement.style.background = `linear-gradient(to right, ${spec.colors.selected} ${fromLeftPercent}%, ${spec.colors.normal} ${fromLeftPercent}%)`
					spec.setValue(opt, optIndex, fromLeft)
					spec.onUpdate?.()
				}

				spec.help = spec.help === undefined ? "" : (spec.help += "\n")
				spec.help += "ctrl+click = zero\nshift+click = one"

				result = createSwitchCommon(spec, optElementInit, toggleOption)
			}
			break
	}

	return result
}
