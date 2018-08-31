header data
{
    SPACING := 6
}
LayoutSpacing(SPACING) [
    (
        (GuiData guiData, ElementID id, BoolState local) {
            Bool sign = local($bool)
        } -> (
            name -> sign? "foo" : "bar",
            color -> 0xffdddddd

            partition -> 0.5    
            onPressed -> Watch(
                MessageBox(state, "foo", "bar"), GuiState prev -> Bool result -> Wrap(prev, prev(id)($bool) = result)
            )
        )
    ) inherits GuiButton(BoolState),
    (  
        (GuiData guiData, ElementID id, BoolState local) {
            Bool sign = local($bool0)
        } -> (
            name -> sign? "f" : "b"
            color -> 0xffdddddd

            partition -> 1.0
            onPressed -> GuiState prev -> Wrap(prev, prev(id)($bool) = !sign)
        )
    ) inherits GuiButton(BoolState)
]