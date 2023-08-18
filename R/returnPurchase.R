#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnPurchaseEcsServer()
returnPurchaseEcsServer <- function(input,output,session,dms_token) {
  var_txt_returnPurchaseSourceSync_manually <- tsui::var_text('txt_returnPurchaseSourceSync_manually')
  var_returnPurchaseERP=tsui::var_ListChoose1('returnPurchaseERP')
  # var_returnPurchaseERP() <- reactive({
  #   switch(input$returnPurchaseERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_returnPurchaseSourceSync_auto=tsui::var_date('date_returnPurchaseSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_returnPurchaseSourceSync_log,{
    FNumber <- var_txt_returnPurchaseSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入采购退料单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::returnPurchaseLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_returnPurchaseSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_returnPurchaseSourceSync_update,{

    FNumber <- var_txt_returnPurchaseSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购退料单号")

    }else{
      token= dms_token

      FName <- var_returnPurchaseERP()
      mdlCpEcsBillr::returnPurchaseStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_returnPurchaseSourceSync_manually,{
    FNumber <- var_txt_returnPurchaseSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购退料单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_returnPurchaseSourceSync_manually()
      FName <- var_returnPurchaseERP()
      mdlCpEcsBillr::returnPurchaseByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$returnPurchaseByNumber_query,{
    FNumber <- var_txt_returnPurchaseSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购退料单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::returnPurchaseByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_returnPurchaseSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_returnPurchaseSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_returnPurchaseSourceSync_auto()
    FName <- var_returnPurchaseERP()
    mdlCpEcsBillr::returnPurchaseByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_returnPurchaseSourceSync_query,{

    token= dms_token
    FStartDate=var_date_returnPurchaseSourceSync_auto()

    data=mdlCpEcsBillr::returnPurchaseByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_returnPurchaseSourceSync_dataView',data = data )



  })

}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnPurchaseErpServer()
returnPurchaseErpServer <- function(input,output,session,dms_token) {
  var_returnPurchaseERP2=tsui::var_ListChoose1('returnPurchaseERP2')

  # var_returnPurchaseERP() <- reactive({
  #   switch(input$returnPurchaseERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_returnPurchaseERP_manually=tsui::var_text('txt_returnPurchaseERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_returnPurchaseERP_manually,{
    FNumber=var_txt_returnPurchaseERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入采购退料单号")

    }else{
      token= dms_token

      FName=var_returnPurchaseERP2()
      data=mdlCpEcsBillr::returnPurchaseErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_returnPurchaseSourceSync_dataView',data = data )



    }




  })


}







#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' returnPurchaseServer()
returnPurchaseServer <- function(input,output,session,dms_token) {
  #演示功用1
  returnPurchaseEcsServer(input,output,session,dms_token)
  returnPurchaseErpServer(input,output,session,dms_token)


}
