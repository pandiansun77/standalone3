      ******************************************************************        
      *                                                                *        
      * LICENSED MATERIALS - PROPERTY OF IBM                           *        
      *                                                                *        
      * "RESTRICTED MATERIALS OF IBM"                                  *        
      *                                                                *        
      * CB12                                                           *        
      *                                                                *        
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *        
      *                                                                *        
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *        
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *        
      * CONTRACT WITH IBM CORPORATION                                  *        
      *                                                                *        
      *                                                                *        
      *               COPYBOOK for COMMAREA structure                  *        
      *                                                                *        
      *   This commarea can be used for all functions                  *        
      *                                                                *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
           03 CA-REQUEST-ID            PIC X(6).                                
           03 CA-RETURN-CODE           PIC 9(2).                                
           03 CA-CUSTOMER-NUM          PIC 9(10).                               
      *    Fields used in INQ, UPD, ADD & DELETE policy                         
           03 CA-POLICY-REQUEST.                                                
              05 CA-POLICY-NUM         PIC 9(10).                               
      *       Common policy details                                             
              05 CA-POLICY-COMMON.                                              
                 07 CA-ISSUE-DATE      PIC X(10).                               
                 07 CA-EXPIRY-DATE     PIC X(10).                               
                 07 CA-LASTCHANGED     PIC X(26).                               
                 07 CA-BROKERID        PIC 9(10).                               
                 07 CA-BROKERSREF      PIC X(10).                               
                 07 CA-PAYMENT         PIC 9(6).                                
      *       House policy description                                          
              05 CA-HOUSE.                                                      
                 07 CA-H-PROPERTY-TYPE   PIC X(15).                             
                 07 CA-H-BEDROOMS        PIC 9(3).                              
                 07 CA-H-VALUE           PIC 9(8).                              
                 07 CA-H-HOUSE-NAME      PIC X(20).                             
                 07 CA-H-HOUSE-NUMBER    PIC X(4).                              
                 07 CA-H-POSTCODE        PIC X(8).                              
                 07 CA-H-FILLER          PIC X(32342).                          
