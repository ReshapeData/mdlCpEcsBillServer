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
#' purchaseOrderEcsServer()
purchaseOrderEcsServer <- function(input,output,session,dms_token) {
  var_txt_purchaseOrderSourceSync_manually <- tsui::var_text('txt_purchaseOrderSourceSync_manually')
  var_purchaseOrderERP=tsui::var_ListChoose1('purchaseOrderERP')
  # var_purchaseOrderERP() <- reactive({
  #   switch(input$purchaseOrderERP,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })
  var_date_purchaseOrderSourceSync_auto=tsui::var_date('date_purchaseOrderSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_purchaseOrderSourceSync_log,{
    FNumber <- var_txt_purchaseOrderSourceSync_manually()
    if(FNumber ==""){
      tsui::pop_notice("请输入采购订单号")

    }else{
      token= dms_token



      data=mdlCpEcsBillr::purchaseOrderLog_query(token=token,FNumber=FNumber)
      tsui::run_dataTable2(id ='dt_purchaseOrderSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_purchaseOrderSourceSync_update,{

    FNumber <- var_txt_purchaseOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购订单号")

    }else{
      token= dms_token

      FName <- var_purchaseOrderERP()
      mdlCpEcsBillr::purchaseOrderStatus_upload(token=token,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_purchaseOrderSourceSync_manually,{
    FNumber <- var_txt_purchaseOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购订单号")

    }else{
      Ftoken= dms_token

      FNumber <- var_txt_purchaseOrderSourceSync_manually()
      FName <- var_purchaseOrderERP()
      mdlCpEcsBillr::purchaseOrderByNumber_sync(Ftoken=Ftoken,FNumber=FNumber,FName=FName)
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$purchaseOrderByNumber_query,{
    FNumber <- var_txt_purchaseOrderSourceSync_manually()

    if(FNumber ==""){
      tsui::pop_notice("请输入采购订单号")

    }else{
      token= dms_token
      data=mdlCpEcsBillr::purchaseOrderByNumber_query(token = token,FNumber = FNumber)

      tsui::run_dataTable2(id ='dt_purchaseOrderSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_purchaseOrderSourceSync_auto,{

    Ftoken= dms_token
    FDate=var_date_purchaseOrderSourceSync_auto()
    FName <- var_purchaseOrderERP()
    mdlCpEcsBillr::purchaseOrderByDate_sync(Ftoken=Ftoken,FDate=FDate,FName=FName)
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_purchaseOrderSourceSync_query,{

    token= dms_token
    FStartDate=var_date_purchaseOrderSourceSync_auto()

    data=mdlCpEcsBillr::purchaseOrderByDate_query(token = token,FStartDate =FStartDate )
    tsui::run_dataTable2(id ='dt_purchaseOrderSourceSync_dataView',data = data )



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
#' purchaseOrderErpServer()
purchaseOrderErpServer <- function(input,output,session,dms_token) {
  var_purchaseOrderERP2=tsui::var_ListChoose1('purchaseOrderERP2')

  # var_purchaseOrderERP() <- reactive({
  #   switch(input$purchaseOrderERP2,
  #          "赛普集团新账套" = 赛普集团新账套,
  #          "测试账套" = 测试账套)
  # })

  var_txt_purchaseOrderERP_manually=tsui::var_text('txt_purchaseOrderERP_manually')
  #按单查询
  shiny::observeEvent(input$btn_purchaseOrderERP_manually,{
    FNumber=var_txt_purchaseOrderERP_manually()
    if(FNumber == ""){
      tsui::pop_notice("请输入采购订单号")

    }else{
      token= dms_token

      FName=var_purchaseOrderERP2()
      data=mdlCpEcsBillr::purchaseOrderErpDataByFNumber_query(token=token,FNumber=FNumber,FName=FName)
      tsui::run_dataTable2(id ='dt_purchaseOrderSourceSync_dataView',data = data )



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
#' purchaseOrderServer()
purchaseOrderServer <- function(input,output,session,dms_token) {
  #演示功用1
  purchaseOrderEcsServer(input,output,session,dms_token)
  purchaseOrderErpServer(input,output,session,dms_token)


}
